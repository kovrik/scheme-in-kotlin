package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException

object Unquote : SpecialForm("unquote") {

    // Implemented in quasiquote
    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Nothing {
        throw IllegalSyntaxException(toString(), form, "not in quasiquote")
    }
}
