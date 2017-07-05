package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.utils.Utils

/* Syntax:
 * (unless <test> body...)
 */
enum class Unless : ISpecialForm {
    UNLESS;

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = form.size
        if (size < 3) {
            throw IllegalSyntaxException.of(toString(), form, "has ${size - 1} parts after keyword")
        }
        val test = form[1]
        if (!Utils.toBoolean(evaluator.eval(test, env))) {
            for (i in 2..form.size - 2) {
                evaluator.eval(form[i], env)
            }
            return Thunk(form[form.size - 1], env)
        }
        return Unit
    }

    override fun toString() = "unless"
}
