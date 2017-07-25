package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

/* Syntax:
 * (set! <variable> <expression>)
 */
object Set : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) {
        when {
            form.size != 3 -> throw IllegalSyntaxException.of(toString(), form, "has ${form.size - 1} parts after keyword")
            form[1] is Symbol -> env.findAndPut(form[1], evaluator.eval(form[2], env))
            else -> throw IllegalSyntaxException.of(toString(), form, "not an identifier: `${form[1]}`")
        }
    }

    override fun toString() = "set!"
}
