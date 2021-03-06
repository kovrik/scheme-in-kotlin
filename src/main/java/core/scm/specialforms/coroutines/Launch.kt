package core.scm.specialforms.coroutines

import core.environment.Environment
import core.Evaluator
import core.Writer
import core.exceptions.IllegalSyntaxException
import core.scm.specialforms.Begin
import core.scm.specialforms.SpecialForm
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

/**
 * Syntax:
 * (launch <expr1> ...)
 */
object Launch : SpecialForm("launch") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = runBlocking {
        when (form.size) {
            1 -> throw IllegalSyntaxException(toString(), Writer.write(form))
            2 -> launch { evaluator.eval(form[1], env) }
            else -> launch { evaluator.eval(listOf(Begin) + form.drop(1), env) }
        }
    }
}