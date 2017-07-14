package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (or <test1> ...)
 */
object Or : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        var result: Any? = false
        if (form.size > 1) {
            for (i in 1..form.size - 2) {
                result = evaluator.eval(form[i], env)
                if (Utils.toBoolean(result)) {
                    return result
                }
            }
            result = Thunk(form[form.size - 1], env)
        }
        return result
    }

    override fun toString() = "or"
}
