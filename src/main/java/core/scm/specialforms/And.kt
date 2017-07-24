package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (and <test1> ...)
 */
object And : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size > 1) {
            for (i in 1..form.size - 2) {
                if (!Utils.toBoolean(evaluator.eval(form[i], env))) {
                    return false
                }
            }
            return Thunk(form[form.size - 1], env)
        }
        return true
    }

    override fun toString() = "and"
}
