package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.utils.Utils
import core.Writer

/**
 * Syntax:
 * (unless <test> body...)
 */
object Unless : SpecialForm("unless") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? = when {
        form.size < 3 -> throw IllegalSyntaxException(name, Writer.write(form), "has ${form.size - 1} parts after keyword")
        !Utils.toBoolean(evaluator.eval(form[1], env)) -> {
            (2..form.size - 2).forEach { evaluator.eval(form[it], env) }
            Thunk(form[form.size - 1], env)
        }
        else -> Unit
    }
}
