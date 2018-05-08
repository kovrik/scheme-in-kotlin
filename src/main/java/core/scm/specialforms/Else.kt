package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.Writer

object Else : SpecialForm("else") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Nothing {
        throw IllegalSyntaxException(toString(), Writer.write(form), "not allowed as an expression")
    }
}
