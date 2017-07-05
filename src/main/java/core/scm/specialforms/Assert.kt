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

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 2 || form.size > 3) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        val result = evaluator.eval(form[1], env)
        if (!Utils.toBoolean(result)) {
            var message = ""
            if (form.size == 3) {
                if (form[2] !is CharSequence) {
                    throw IllegalSyntaxException.of(toString(), form)
                }
                message = ": ${form[2].toString()}"
            }
            val assertionError = AssertionError("assert failed $message")
            assertionError.stackTrace = EMPTY
            throw assertionError
        }
        return true
    }

    override fun toString() = "assert"
}
