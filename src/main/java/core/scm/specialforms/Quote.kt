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
enum class Quote : ISpecialForm {
    QUOTE;

    companion object {

        val QUOTE_SYMBOL: Symbol = Symbol.intern(QUOTE.toString())

        fun quote(obj: Any) = Cons.list(QUOTE, obj)
    }

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator) = expression[1]

    override fun toString() = "quote"
}
