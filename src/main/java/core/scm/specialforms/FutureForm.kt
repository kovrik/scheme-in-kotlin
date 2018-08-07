package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Future
import core.Writer

/* Syntax:
 * (future <expression>)
 */
object FutureForm : SpecialForm("future") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), Writer.write(form))
        2    -> form[1]
        else -> listOf(Begin) + form.drop(1)
    }.let { Future(it, env, evaluator).apply { Evaluator.executor.submit(this) } }
}
