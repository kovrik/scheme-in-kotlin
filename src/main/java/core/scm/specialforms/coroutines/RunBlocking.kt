package core.scm.specialforms.coroutines

import core.environment.Environment
import core.Evaluator
import core.Writer
import core.exceptions.IllegalSyntaxException
import core.scm.specialforms.Begin
import core.scm.specialforms.SpecialForm
import kotlinx.coroutines.runBlocking

/**
 * Syntax:
 * (run-blocking <expr1> ...)
 */
object RunBlocking : SpecialForm("run-blocking") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), Writer.write(form))
        2    -> runBlocking { evaluator.eval(form[1], env) }
        else -> runBlocking { evaluator.eval(listOf(Begin) + form.drop(1), env) }
    }
}