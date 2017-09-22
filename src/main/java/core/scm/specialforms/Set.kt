package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol
import core.writer.Writer

/* Syntax:
 * (set! <variable> <expression>)
 */
object Set : SpecialForm("set!") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) {
        when {
            form.size != 3 -> throw IllegalSyntaxException(toString(), Writer.write(form), "has ${form.size - 1} parts after keyword")
            form[1] is Symbol -> env.findAndPut(form[1], evaluator.eval(form[2], env))
            else -> throw IllegalSyntaxException(toString(), Writer.write(form), "not an identifier: `${form[1]}`")
        }
    }
}
