package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.scm.Cons
import core.scm.Symbol

/* Literal expressions
 * Syntax:
 * (quote <datum>)
 * '<datum>
 * <constant>
 */
object Quote : ISpecialForm {

    val QUOTE_SYMBOL = Symbol.intern(toString())

    fun quote(obj: Any) = Cons.list(this, obj)

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = form[1]

    override fun toString() = "quote"
}
