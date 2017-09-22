package core.scm.specialforms

import core.environment.Environment
import core.Evaluator

/* Literal expressions
 * Syntax:
 * (quote <datum>)
 * '<datum>
 * <constant>
 */
object Quote : SpecialForm("quote") {

    fun quote(obj: Any) = listOf(this, obj)

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = form[1]
}
