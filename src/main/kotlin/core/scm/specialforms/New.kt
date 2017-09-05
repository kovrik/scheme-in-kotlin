package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.Reflector
import core.exceptions.IllegalSyntaxException

object New : SpecialForm("new") {

    private val reflector = Reflector()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 2) {
            throw IllegalSyntaxException(toString(), form)
        }
        val constructorArgs = arrayOfNulls<Any>(form.size - 2)
        constructorArgs.indices.forEach { i -> constructorArgs[i] = evaluator.eval(form[i + 2], env) }
        val clazz = form[1].toString()
        return reflector.newInstance(clazz, constructorArgs)
    }
}
