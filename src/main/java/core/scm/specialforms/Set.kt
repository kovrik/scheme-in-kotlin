package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

/* Syntax:
 * (set! <variable> <expression>)
 */
enum class Set : ISpecialForm {
    SET;

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) {
        if (form.size != 3) {
            throw IllegalSyntaxException.of(toString(), form, "has ${form.size - 1} parts after keyword")
        }
        val identifier = form[1]
        if (identifier !is Symbol) {
            throw IllegalSyntaxException.of(toString(), form, "not an identifier: `$identifier`")
        }
        env.findAndPut(identifier, evaluator.eval(form[2], env))
        return Unit
    }

    override fun toString() = "set!"
}
