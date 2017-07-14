package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

object UnquoteSplicing : ISpecialForm {

    val UNQUOTE_SPLICING_SYMBOL = Symbol.intern(toString())

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        // Implemented in quasiquote
        throw IllegalSyntaxException.of(toString(), form, "not in quasiquote")
    }

    override fun toString() = "unquote-splicing"
}
