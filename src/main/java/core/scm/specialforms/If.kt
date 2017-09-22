package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.utils.Utils
import core.writer.Writer

/* Syntax:
 * (if <test> <consequent> <alternate>)
 * (if <test> <consequent>)
 */
object If : SpecialForm("if") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = form.size
        if (size != 4) {
            throw IllegalSyntaxException(toString(), Writer.write(form), "has ${size - 1} parts after keyword")
        }
        return if (Utils.toBoolean(evaluator.eval(form[1], env)))
            Thunk(form[2], env)
        else
            Thunk(form[3], env)
    }
}
