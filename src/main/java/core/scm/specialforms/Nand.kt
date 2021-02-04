package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.procedures.booleans.Not
import core.scm.Thunk
import core.utils.Utils

/**
 * Syntax:
 * (nand <test1> ...)
 */
object Nand : SpecialForm("nand") {

    private val not = Not()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size > 1) {
            (1..form.size - 2).forEach {
                if (!Utils.toBoolean(evaluator.eval(form[it], env))) {
                    return true
                }
            }
            return Thunk(listOf(not, form[form.size - 1]), env)
        }
        return false
    }
}