package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (when <test> body...)
 */
object When : SpecialForm("when") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = form.size
        if (size < 3) {
            throw IllegalSyntaxException(toString(), form, "has ${size - 1} parts after keyword")
        }
        if (Utils.toBoolean(evaluator.eval(form[1], env))) {
            for (i in 2..size - 2) { evaluator.eval(form[i], env) }
            return Thunk(form[size - 1], env)
        }
        return Unit
    }
}
