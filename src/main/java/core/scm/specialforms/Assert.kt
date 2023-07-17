package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.utils.Utils
import core.Writer

object Assert : SpecialForm("assert") {

    private val EMPTY = emptyArray<StackTraceElement>()

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any {
        val message = when (form.size) {
            2 -> "assert failed"
            3 -> "assert failed: \"${evaluator.eval(form[2]) as? CharSequence ?:
                 throw IllegalSyntaxException(toString(), Writer.write(form))}\""
            else -> throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        if (!Utils.toBoolean(evaluator.eval(form[1]))) {
            throw AssertionError(message).apply { stackTrace = EMPTY }
        }
        return true
    }
}
