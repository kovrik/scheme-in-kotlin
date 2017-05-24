package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (if <test> <consequent> <alternate>)
 * (if <test> <consequent>)
 */
enum class If : ISpecialForm {
    IF;

    override fun eval(expression: List<*>, env: Environment, evaluator: Evaluator): Any? {
        val size = expression.size
        if (size != 4) {
            throw IllegalSyntaxException
                    .of(toString(), expression, String.format("has %s parts after keyword", size - 1))
        }
        return if (Utils.toBoolean(evaluator.eval(expression[1], env)))
            Thunk(expression[2], env)
        else
            Thunk(expression[3], env)
    }

    override fun toString(): String {
        return "if"
    }
}
