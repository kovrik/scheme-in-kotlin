package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

enum class Else : ISpecialForm {
    ELSE;

    companion object {
        val ELSE_SYMBOL = Symbol.intern("else")
    }

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        throw IllegalSyntaxException.of(toString(), form, "not allowed as an expression")
    }

    override fun toString() = "else"
}
