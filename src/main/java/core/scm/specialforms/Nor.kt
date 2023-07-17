package core.scm.specialforms

import core.Evaluator
import core.procedures.booleans.Not
import core.scm.Thunk
import core.utils.Utils

/**
 * Syntax:
 * (nor <test1> ...)
 */
object Nor : SpecialForm("nor") {

    private val not = Not()

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any {
        if (form.size > 1) {
            (1..form.size - 2).forEach {
                val result = evaluator.eval(form[it])
                if (Utils.toBoolean(result)) {
                    return false
                }
            }
            return Thunk(listOf(not, form[form.size - 1]), evaluator.env)
        }
        return true
    }
}