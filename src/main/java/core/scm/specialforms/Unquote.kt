package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

object Unquote : ISpecialForm {

    val UNQUOTE_SYMBOL = Symbol.intern(toString())

    // Implemented in quasiquote
    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Nothing {
        throw IllegalSyntaxException(toString(), form, "not in quasiquote")
    }

    override fun toString() = "unquote"
}
