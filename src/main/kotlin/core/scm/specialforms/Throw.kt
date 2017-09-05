package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException

object Throw : SpecialForm("throw") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Nothing {
        if (form.size < 2) {
            throw IllegalSyntaxException(toString(), form)
        }
        evaluator.eval(form[1], env).let {
            when (it) {
                is Throwable -> throw it
                else         -> throw WrongTypeException(toString(), "Throwable", it)
            }
        }
    }
}
