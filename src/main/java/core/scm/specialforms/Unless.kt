package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (unless <test> body...)
 */
enum class Unless : ISpecialForm {
    UNLESS;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = expression.size
        if (size < 3) {
            throw IllegalSyntaxException.of(toString(), expression, "has ${size - 1} parts after keyword")
        }
        val test = expression[1]
        if (!Utils.toBoolean(evaluator.eval(test, env))) {
            for (i in 2..expression.size - 2) {
                evaluator.eval(expression[i], env)
            }
            return Thunk(expression[expression.size - 1], env)
        }
        return Unit
    }

    override fun toString() = "unless"
}
