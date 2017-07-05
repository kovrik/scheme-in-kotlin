package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.ThrowableWrapper
import core.exceptions.WrongTypeException

enum class Throw : ISpecialForm {
    THROW;

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 2) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        val obj = evaluator.eval(form[1], env)
        if (obj !is Throwable) {
            throw WrongTypeException(toString(), "Throwable", obj)
        }
        throw ThrowableWrapper((obj as Throwable?)!!)
    }

    override fun toString() = "throw"
}
