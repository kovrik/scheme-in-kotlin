package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.writer.Writer

object Unquote : SpecialForm("unquote") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Nothing {
        // Implemented in quasiquote
        throw IllegalSyntaxException(toString(), Writer.write(form), "not in quasiquote")
    }
}
