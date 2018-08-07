package core.scm.specialforms.coroutines

import core.environment.Environment
import core.Evaluator
import core.Writer
import core.exceptions.IllegalSyntaxException
import core.scm.specialforms.Begin
import core.scm.specialforms.SpecialForm
import kotlinx.coroutines.experimental.async

/* Syntax:
 * (async <expr1> ...)
 */
object Async : SpecialForm("async") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), Writer.write(form))
        2    -> async { evaluator.eval(form[1], env) }
        else -> async { evaluator.eval(listOf(Begin) + form.drop(1), env) }
    }
}