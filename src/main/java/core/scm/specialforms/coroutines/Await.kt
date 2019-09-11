package core.scm.specialforms.coroutines

import core.Evaluator
import core.Writer
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.scm.specialforms.SpecialForm
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.runBlocking

/**
 * Syntax:
 * (await <deferred1>)
 */
object Await : SpecialForm("await") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = runBlocking {
        when (form.size == 2) {
            true -> evaluator.eval(form[1], env).let {
                it as? Deferred<Any?> ?: throw WrongTypeException(Await.name, "Coroutine", it)
            }.await()
            false -> throw IllegalSyntaxException(toString(), Writer.write(form))
        }
    }
}