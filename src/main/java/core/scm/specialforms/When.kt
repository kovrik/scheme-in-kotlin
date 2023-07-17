package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.utils.Utils
import core.Writer

/**
 * Syntax:
 * (when <test> body...)
 */
object When : SpecialForm("when") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? = when {
        form.size == 2 -> null
        form.size < 2 -> throw IllegalSyntaxException(toString(), Writer.write(form), "has ${form.size - 1} parts after keyword")
        Utils.toBoolean(evaluator.eval(form[1])) -> {
            (2..form.size - 2).forEach { evaluator.eval(form[it] ) }
            Thunk(form[form.size - 1], evaluator.env)
        }
        else -> Unit
    }
}
