package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol
import core.scm.Thunk
import core.Writer
import core.scm.Type

/* Syntax:
 * (let <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
object Let : SpecialForm("let") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        /* Normal let:
         * (let ((id expr) ...) body ...+) */
        if (form[1] is List<*>) {
            val localEnv = Environment(env)
            val bindings = form[1] as List<*>
            /* Bind variables to fresh locations holding undefined values */
            bindings.forEach {
                if (it !is List<*>) throw IllegalSyntaxException(toString(), Writer.write(form))
                localEnv[it[0]] = Type.Undefined
            }
            /* Evaluate inits */
            bindings.forEach {
                when {
                    it !is List<*> -> throw IllegalSyntaxException(toString(), Writer.write(form))
                    localEnv[it[0]] === Type.Undefined -> localEnv[it[0]] = evaluator.eval(it[1], env)
                    else -> throw IllegalSyntaxException(toString(), Writer.write(form), "duplicate identifier: ${it[0]}")
                }
            }
            /* Evaluate body */
            for (i in 2..form.size - 2) { evaluator.eval(form[i], localEnv) }
            return Thunk(form[form.size - 1], localEnv)
        } else if (form[1] is Symbol) {
            // TODO Optimize and cleanup
            /* Named let:
             * (let proc-id ((arg-id init-expr) ...) body ...+) */
            val name = form[1] as? Symbol ?: throw IllegalSyntaxException(toString(), Writer.write(form))
            /* Construct lambda */
            val lambdaArgs = mutableListOf<Any?>()
            val initValues = mutableListOf<Any?>()
            val bindings = form[2] as List<*>
            for (binding in bindings) {
                val (arg, init) = binding as List<*>
                if (arg in lambdaArgs) {
                    throw IllegalSyntaxException(toString(), Writer.write(form), "duplicate identifier: $arg")
                }
                lambdaArgs.add(arg)
                initValues.add(init)
            }
            val lambdaBody = form[3]
            val lambda = listOf(Lambda, lambdaArgs, lambdaBody)
            val body = listOf(name).plus(initValues)
            /* Named let is implemented via letrec (letrec has TCO) */
            return LetRec.eval(listOf(LetRec, listOf(listOf(name, lambda)), body), Environment(env), evaluator)
        }
        throw IllegalSyntaxException(toString(), Writer.write(form))
    }
}
