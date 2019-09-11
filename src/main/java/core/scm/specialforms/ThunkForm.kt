package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Closure
import core.Writer
import core.procedures.Arity.Exactly

/**
 * Syntax:
 * (thunk <body> ...)
 *
 * Returns a nullary function (lambda) that evaluates the given body.
 */
object ThunkForm : SpecialForm("thunk") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Closure {
        val body = when (form.size) {
            1 -> throw IllegalSyntaxException(toString(), Writer.write(form))
            2 -> form[1]
            else -> listOf(Begin) + form.drop(1)
        }
        return Closure(emptyList<Nothing>(), body, env, Exactly(0))
    }
}
