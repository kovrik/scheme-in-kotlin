package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.evaluator.Reflector
import core.exceptions.IllegalSyntaxException
import core.exceptions.ThrowableWrapper
import core.scm.Cons
import core.scm.Symbol

object Try : ISpecialForm {

    private val REFLECTOR = Reflector()
    private val CATCH = Symbol.intern("catch")
    private val FINALLY = Symbol.intern("finally")

    override fun toString() = "try"

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.isEmpty()) {
            return null
        }
        var hadCatch = false
        val catches = LinkedHashMap<Class<*>, Any?>()
        var catchBindings: MutableMap<Class<*>, Symbol> = HashMap()
        var fin: Any? = null
        val expressions = ArrayList<Any?>()
        /* Init and check syntax */
        for (i in 1..form.size - 1) {
            val e = form[i]
            if (e is List<*> && !e.isEmpty()) {
                val expr = e
                val op = expr[0]
                if (FINALLY == op) {
                    if (i != form.size - 1) {
                        throw IllegalSyntaxException("try: finally clause must be last in try expression")
                    }
                    if (expr.size > 1) {
                        fin = Cons.list(Begin)
                        (fin as Cons<Any?>).addAll(expr.subList(1, expr.size))
                    }
                    continue
                } else if (CATCH == op) {
                    if (expr.size < 3) {
                        throw IllegalSyntaxException("catch: bad syntax in form: $expr")
                    }
                    hadCatch = true
                    if (catches.isEmpty()) {
                        catchBindings = HashMap<Class<*>, Symbol>()
                    }
                    val clazz = REFLECTOR.getClazz(expr[1].toString())
                    var catchExpr: Any? = null
                    if (expr.size > 3) {
                        catchExpr = Cons.list(Begin)
                        (catchExpr as Cons<Any?>).addAll(expr.subList(3, expr.size))
                    }
                    catches.put(clazz, catchExpr)
                    val cb = expr[2]
                    if (cb !is Symbol) {
                        throw IllegalSyntaxException("catch: bad binding form, expected Symbol, actual: $cb")
                    }
                    catchBindings.put(clazz, cb)
                    continue
                } else {
                    if (hadCatch) {
                        throw IllegalSyntaxException("try: only catch or finally clause can follow catch in try expression")
                    }
                }
            }
            expressions.add(e)
        }
        /* Now Evaluate everything */
        try {
            var result: Any? = null
            for (e in expressions) {
                result = evaluator.eval(e, env)
            }
            return result
        } catch (e: Throwable) {
            /* Unwrap if it is a ThrowableWrapper */
            val ex = if (e is ThrowableWrapper) e.cause else e
            /* Check if we had catch block for that type of exception (OR for any superclass) */
            for (clazz in catches.keys) {
                if (clazz.isAssignableFrom(ex!!.javaClass)) {
                    /* Bind exception */
                    env.put(catchBindings[clazz], e)
                    /* Evaluate corresponding catch block */
                    return evaluator.eval(catches[clazz], env)
                }
            }
            /* Unexpected exception, re-throw it */
            throw ThrowableWrapper(e)
        } finally {
            /* And finally, evaluate finally block (if present) */
            if (fin != null) {
                evaluator.eval(fin, env)
            }
        }
    }
}
