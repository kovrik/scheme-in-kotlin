package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

enum class Unquote : ISpecialForm {
    UNQUOTE;

    companion object {
        val UNQUOTE_SYMBOL: Symbol = Symbol.intern(UNQUOTE.toString())
    }

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        // Implemented in quasiquote
        throw IllegalSyntaxException.of(toString(), expression, "not in quasiquote")
    }

    override fun toString() = "unquote"
}
