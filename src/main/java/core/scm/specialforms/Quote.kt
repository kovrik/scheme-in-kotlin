package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.Writer
import core.exceptions.IllegalSyntaxException

/**
 * Syntax:
 * (quote <datum>)
 * '<datum>
 * <constant>
 */
object Quote : SpecialForm("quote") {

    fun quote(obj: Any) = listOf(this, obj)

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size == 2) {
        true  -> form[1]
        false -> throw IllegalSyntaxException(toString(), Writer.write(form))
    }
}
