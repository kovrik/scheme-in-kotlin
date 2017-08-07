package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException

object UnquoteSplicing : SpecialForm("unquote-splicing") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Nothing {
        // Implemented in quasiquote
        throw IllegalSyntaxException(toString(), form, "not in quasiquote")
    }
}
