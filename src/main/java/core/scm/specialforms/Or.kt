package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.scm.Thunk
import core.utils.Utils

/**
 * Syntax:
 * (or <test1> ...)
 */
object Or : SpecialForm("or") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size > 1) {
            (1..form.size - 2).forEach {
                val result = evaluator.eval(form[it], env)
                if (Utils.toBoolean(result)) {
                    return result
                }
            }
            return Thunk(form[form.size - 1], env)
        }
        return false
    }
}
