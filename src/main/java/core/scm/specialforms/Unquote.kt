package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.Writer

object Unquote : SpecialForm("unquote") {

    // Implemented in quasiquote
    override fun eval(form: List<Any?>, evaluator: Evaluator) =
            throw IllegalSyntaxException(toString(), Writer.write(form), "not in quasiquote")
}
