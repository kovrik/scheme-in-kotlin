package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.Writer

object Throw : SpecialForm("throw") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size < 2) {
        true  -> throw IllegalSyntaxException(toString(), Writer.write(form))
        false -> evaluator.eval(form[1], env).let { throw it as? Throwable ?: WrongTypeException(toString(), "Throwable", it) }
    }

}
