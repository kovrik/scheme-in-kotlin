package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol
import core.scm.Thunk
import core.Writer

/**
 * Syntax:
 * (let <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
object Let : SpecialForm("let") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        /* Normal let:
         * (let ((id expr) ...) body ...+) */
        if (form[1] is List<*>) {
            val localEvaluator = Evaluator(Environment(evaluator.env))
            val bindings = form[1] as List<*>
            /* Bind variables to fresh locations holding undefined values */
            bindings.forEach {
                if (it !is List<*>) throw IllegalSyntaxException(toString(), Writer.write(form))
                localEvaluator.env[it[0]] = Unit
            }
            /* Evaluate inits */
            bindings.forEach {
                when {
                    it !is List<*> -> throw IllegalSyntaxException(toString(), Writer.write(form))
                    localEvaluator.env[it[0]] === Unit -> localEvaluator.env[it[0]] = evaluator.eval(it[1])
                    else -> throw IllegalSyntaxException(toString(), Writer.write(form), "duplicate identifier: ${it[0]}")
                }
            }
            /* Evaluate body */
            (2..form.size - 2).forEach { localEvaluator.eval(form[it]) }
            return Thunk(form[form.size - 1], localEvaluator.env)
        } else if (form[1] is Symbol) {
            // TODO Optimize and cleanup
            /* Named let:
             * (let proc-id ((arg-id init-expr) ...) body ...+) */
            val name = form[1] as? Symbol ?: throw IllegalSyntaxException(toString(), Writer.write(form))
            /* Construct lambda */
            val lambdaArgs = mutableListOf<Any?>()
            val initValues = mutableListOf<Any?>()
            val bindings = form[2] as List<*>
            bindings.forEach { binding ->
                val (arg, init) = binding as List<*>
                if (arg in lambdaArgs) {
                    throw IllegalSyntaxException(toString(), Writer.write(form), "duplicate identifier: $arg")
                }
                lambdaArgs.add(arg)
                initValues.add(init)
            }
            val lambdaBody = form[3]
            val lambda = listOf(Lambda, lambdaArgs, lambdaBody)
            val body = listOf(name) + initValues
            /* Named let is implemented via letrec (letrec has TCO) */
            return LetRec.eval(listOf(LetRec, listOf(listOf(name, lambda)), body), Evaluator(Environment(evaluator.env)))
        }
        throw IllegalSyntaxException(toString(), Writer.write(form))
    }
}
