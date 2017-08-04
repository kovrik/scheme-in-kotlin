package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.utils.Utils

object Assert : ISpecialForm {

    private val EMPTY = arrayOfNulls<StackTraceElement>(0)

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val message = when (form.size) {
            2 -> "assert failed"
            3 -> "assert failed: \"${evaluator.eval(form[2], env) as? CharSequence ?:
                 throw IllegalSyntaxException(toString(), form)}\""
            else -> throw IllegalSyntaxException(toString(), form)
        }
        if (!Utils.toBoolean(evaluator.eval(form[1], env))) {
            throw AssertionError(message).apply { stackTrace = EMPTY }
        }
        return true
    }

    override fun toString() = "assert"
}
