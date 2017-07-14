package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.evaluator.Reflector
import core.exceptions.IllegalSyntaxException

object New : ISpecialForm {

    private val reflector = Reflector()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 2) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        val constructorArgs = arrayOfNulls<Any>(form.size - 2)
        for (i in constructorArgs.indices) {
            constructorArgs[i] = evaluator.eval(form[i + 2], env)
        }
        val clazz = form[1].toString()
        return reflector.newInstance(clazz, constructorArgs)
    }

    override fun toString() = "new"
}
