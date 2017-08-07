package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Procedure
import core.scm.Thunk

/* Syntax:
 * (thunk <body> ...)
 */
object ThunkForm : SpecialForm("thunk") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 2) {
            throw IllegalSyntaxException(toString(), form)
        }
        /* Return a nullary function (lambda) that evaluates the given body */
        val body = when (form.size) {
            2    -> form[1]
            else -> Cons.list<Any>(Begin).apply { addAll(form.subList(1, form.size)) }
        }
        return Procedure("", emptyArray(), Thunk(body, env), env, false)
    }
}
