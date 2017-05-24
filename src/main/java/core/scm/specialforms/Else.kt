package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

enum class Else : ISpecialForm {
    ELSE;

    companion object {
        val ELSE_SYMBOL: Symbol = Symbol.intern("else")!!
    }

    override fun eval(expression: List<*>, env: Environment, evaluator: Evaluator): Any? {
        throw IllegalSyntaxException.of(toString(), expression, "not allowed as an expression")
    }

    override fun toString(): String {
        return "else"
    }
}
