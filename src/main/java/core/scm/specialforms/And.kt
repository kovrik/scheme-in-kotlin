package core.scm.specialforms

import core.Evaluator
import core.scm.Thunk
import core.utils.Utils

/**
 * Syntax:
 * (and <test1> ...)
 */
object And : SpecialForm("and") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any = when {
        form.size > 1 -> {
            (1..form.size - 2).forEach {
                if (!Utils.toBoolean(evaluator.eval(form[it]))) {
                    return false
                }
            }
            Thunk(form[form.size - 1], evaluator.env)
        }
        else -> true
    }
}
