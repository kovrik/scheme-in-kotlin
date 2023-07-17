package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Lazy
import core.Writer

/**
 * Syntax:
 * (lazy <expression>)
 */
object LazyForm : SpecialForm("lazy") {

    override fun eval(form: List<Any?>, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), Writer.write(form))
        2    -> Lazy(form[1], evaluator)
        else -> Lazy(listOf(Begin) + form.drop(1), evaluator)
    }
}