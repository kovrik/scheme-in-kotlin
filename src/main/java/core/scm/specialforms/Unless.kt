package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.scm.Void
import core.utils.Utils

/* Syntax:
 * (unless <test> body...)
 */
enum class Unless : ISpecialForm {
    UNLESS;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = expression.size
        if (size < 3) {
            throw IllegalSyntaxException.of(toString(), expression, String.format("has %s parts after keyword", size - 1))
        }
        val test = expression[1]
        if (!Utils.toBoolean(evaluator.eval(test, env))) {
            for (i in 2..expression.size - 1 - 1) {
                evaluator.eval(expression[i], env)
            }
            return Thunk(expression[expression.size - 1], env)
        }
        return Void
    }

    override fun toString(): String {
        return "unless"
    }
}
