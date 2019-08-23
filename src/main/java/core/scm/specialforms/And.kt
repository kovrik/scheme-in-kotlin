package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (and <test1> ...)
 */
object And : SpecialForm("and") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? = when {
        form.size > 1 -> {
            (1..form.size - 2).forEach {
                if (!Utils.toBoolean(evaluator.eval(form[it], env))) {
                    return false
                }
            }
            Thunk(form[form.size - 1], env)
        }
        else -> true
    }
}
