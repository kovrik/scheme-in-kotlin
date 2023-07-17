package core.scm.specialforms

import core.Evaluator
import core.Reflector
import core.exceptions.IllegalSyntaxException
import core.Writer

object New : SpecialForm("new") {

    private val reflector = Reflector()

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any {
        if (form.size < 2) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val constructorArgs = arrayOfNulls<Any>(form.size - 2)
        constructorArgs.indices.forEach { i -> constructorArgs[i] = evaluator.eval(form[i + 2]) }
        val clazz = form[1].toString()
        return reflector.newInstance(clazz, constructorArgs)
    }
}
