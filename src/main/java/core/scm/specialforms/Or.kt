package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (or <test1> ...)
 */
enum class Or : ISpecialForm {
    OR;

    override fun eval(expression: List<*>, env: Environment, evaluator: Evaluator): Any? {
        var result: Any? = java.lang.Boolean.FALSE
        if (expression.size > 1) {
            for (i in 1..expression.size - 1 - 1) {
                result = evaluator.eval(expression[i], env)
                if (Utils.toBoolean(result)) {
                    return result
                }
            }
            result = Thunk(expression[expression.size - 1], env)
        }
        return result
    }

    override fun toString(): String {
        return "or"
    }
}
