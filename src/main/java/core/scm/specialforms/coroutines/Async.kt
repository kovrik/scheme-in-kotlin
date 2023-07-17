package core.scm.specialforms.coroutines

import core.Evaluator
import core.Writer
import core.exceptions.IllegalSyntaxException
import core.scm.specialforms.Begin
import core.scm.specialforms.SpecialForm
import kotlinx.coroutines.async
import kotlinx.coroutines.runBlocking

/**
 * Syntax:
 * (async <expr1> ...)
 */
object Async : SpecialForm("async") {

    override fun eval(form: List<Any?>, evaluator: Evaluator) = runBlocking {
        when (form.size) {
            1 -> throw IllegalSyntaxException(toString(), Writer.write(form))
            2 -> async { evaluator.eval(form[1]) }
            else -> async { evaluator.eval(listOf(Begin) + form.drop(1)) }
        }
    }
}