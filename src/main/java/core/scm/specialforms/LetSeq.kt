package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk

/* Syntax:
 * (let* <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
enum class LetSeq : ISpecialForm {
    LETSEQ;

    override fun eval(expression: List<*>, env: Environment, evaluator: Evaluator): Any {
        if (expression.size < 3) {
            throw IllegalSyntaxException.of(toString(), expression)
        }

        val localEnv = Environment(env)
        val bindings = expression[1] as List<*>
        /* Evaluate inits */
        for (binding in bindings) {
            val `var` = (binding as List<*>)[0]
            val init = binding[1]
            localEnv.put(`var`, evaluator.eval(init, localEnv))
        }
        /* Evaluate body */
        for (i in 2..expression.size - 1 - 1) {
            evaluator.eval(expression[i], localEnv)
        }
        return Thunk(expression[expression.size - 1], localEnv)
    }

    override fun toString(): String {
        return "let*"
    }
}
