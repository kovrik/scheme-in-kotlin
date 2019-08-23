package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.Reflector
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol
import core.Writer
import core.scm.Type

object Dot : SpecialForm(".") {

    private val reflector = Reflector()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form), "has ${form.size - 1} parts after keyword")
        }
        val first = when (form[1]) {
            is Symbol -> env.resolve(form[1]).let {
                when (it) {
                    is Type.Undefined -> evaluator.eval(form[1], env)
                    else -> it
                }
            }
            else -> evaluator.eval(form[1], env)
        }
        val methodArgs = arrayOfNulls<Any>(form.size - 3)
        /* Add rest args (if any) */
        (0 until methodArgs.size).forEach { methodArgs[it] = evaluator.eval(form[it + 3], env) }
        if (first is Class<*>) {
            /* (. Classname-symbol member-symbol) */
            /* try static field first */
            return try {
                reflector.evalJavaStaticField(form[1].toString(), form[2].toString())
            } catch (e: NoSuchFieldException) {
                /* (. Classname-symbol method-symbol args) */
                reflector.evalJavaStaticMethod(form[1].toString(), form[2].toString(), methodArgs)
            }
        }
        /* (. instance-expr member-symbol)
         * (. instance-expr -field-symbol)
         * (. instance-expr method-symbol args)
         */
        return reflector.evalJavaInstanceMethod(form[2].toString(), first!!, methodArgs)
    }
}
