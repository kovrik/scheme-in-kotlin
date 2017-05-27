package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.utils.Utils

enum class Assert : ISpecialForm {
    ASSERT;

    companion object {
        private val EMPTY = arrayOfNulls<StackTraceElement>(0)
    }

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (expression.size < 2 || expression.size > 3) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        val result = evaluator.eval(expression[1], env)
        if (!Utils.toBoolean(result)) {
            var message = ""
            if (expression.size == 3) {
                if (expression[2] !is CharSequence) {
                    throw IllegalSyntaxException.of(toString(), expression)
                }
                message = ": " + expression[2].toString()
            }
            val assertionError = AssertionError("assert failed" + message)
            assertionError.stackTrace = EMPTY
            throw assertionError
        }
        return true
    }

    override fun toString(): String {
        return "assert"
    }
}
