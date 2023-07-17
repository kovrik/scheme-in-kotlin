package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Delay
import core.Writer

/**
 * Syntax:
 * (delay <expression>)
 */
object DelayForm : SpecialForm("delay") {

    override fun eval(form: List<Any?>, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), Writer.write(form))
        2    -> Delay(form[1], evaluator)
        else -> Delay(listOf(Begin) + form.drop(1), evaluator)
    }
}
