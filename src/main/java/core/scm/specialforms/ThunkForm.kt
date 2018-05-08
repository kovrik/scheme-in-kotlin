package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Procedure
import core.scm.Thunk
import core.Writer

/* Syntax:
 * (thunk <body> ...)
 */
object ThunkForm : SpecialForm("thunk") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        /* Return a nullary function (lambda) that evaluates the given body */
        val body = when (form.size) {
            1    -> throw IllegalSyntaxException(toString(), Writer.write(form))
            2    -> form[1]
            else -> mutableListOf<Any?>(Begin).apply { addAll(form.subList(1, form.size)) }
        }
        return Procedure("", emptyArray(), Thunk(body, env), env, false)
    }
}
