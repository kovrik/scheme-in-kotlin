package core.scm.specialforms

import core.Evaluator
import core.Reflector
import core.Writer
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

object Try : SpecialForm("try") {

    object Catch : SpecialForm("catch") {
        override fun eval(form: List<Any?>, evaluator: Evaluator): Nothing {
            throw IllegalSyntaxException(toString(), Writer.write(form), "not allowed as an expression")
        }
    }

    object Finally : SpecialForm("finally") {
        override fun eval(form: List<Any?>, evaluator: Evaluator): Nothing {
            throw IllegalSyntaxException(toString(), Writer.write(form), "not allowed as an expression")
        }
    }

    private val reflector = Reflector()

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? {
        if (form.isEmpty()) {
            return null
        }
        var hadCatch = false
        val catches = LinkedHashMap<Class<*>, Any?>()
        var catchBindings = mutableMapOf<Class<*>, Symbol>()
        var fin: Any? = null
        val expressions = mutableListOf<Any?>()
        /* Init and check syntax */
        for (i in 1 until form.size) {
            val expr = form[i]
            if (expr is List<*> && !expr.isEmpty()) {
                val op = evaluator.env.resolve(expr.first())
                if (op == Finally) {
                    if (i != form.size - 1) {
                        throw IllegalSyntaxException("$name: finally clause must be last in try expression")
                    }
                    if (expr.size > 1) {
                        fin = listOf(Begin) + expr.drop(1)
                    }
                    continue
                } else if (op === Catch) {
                    if (expr.size < 3) {
                        throw IllegalSyntaxException("$Catch: bad syntax in form: $expr")
                    }
                    hadCatch = true
                    if (catches.isEmpty()) {
                        catchBindings = HashMap()
                    }
                    val clazz = reflector.getClazz(expr[1].toString())
                    val catchExpr = when {
                        expr.size > 3 -> listOf(Begin) + expr.drop(3)
                        else -> null
                    }
                    catches[clazz] = catchExpr
                    val sym = expr[2] as? Symbol ?:
                              throw IllegalSyntaxException("$Catch: bad binding form, expected Symbol, actual: ${expr[2]}")
                    catchBindings[clazz] = sym
                    continue
                }
            }
            if (hadCatch) {
                throw IllegalSyntaxException("$name: only catch or finally clause can follow catch in try expression")
            }
            expressions.add(expr)
        }
        /* Now Evaluate everything */
        try {
            var result: Any? = null
            expressions.forEach { result = evaluator.eval(it) }
            return result
        } catch (e: Throwable) {
            /* Check if we had catch block for that type of exception (OR for any superclass) */
            for (clazz in catches.keys) {
                if (clazz.isAssignableFrom(e.javaClass)) {
                    /* Bind exception */
                    evaluator.env[catchBindings[clazz]] = e
                    /* Evaluate corresponding catch block */
                    return evaluator.eval(catches[clazz])
                }
            }
            /* Unexpected exception, re-throw it */
            throw e
        } finally {
            /* And finally, evaluate finally block (if present) */
            fin?.let { evaluator.eval(fin) }
        }
    }
}
