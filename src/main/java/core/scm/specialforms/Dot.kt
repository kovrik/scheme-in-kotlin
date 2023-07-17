package core.scm.specialforms

import core.Evaluator
import core.Reflector
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol
import core.Writer

object Dot : SpecialForm(".") {

    private val reflector = Reflector()

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form), "has ${form.size - 1} parts after keyword")
        }
        val first = when (form[1]) {
            is Symbol -> evaluator.env.resolve(form[1]).let {
                when (it) {
                    is Unit -> evaluator.eval(form[1])
                    else -> it
                }
            }
            else -> evaluator.eval(form[1])
        }
        val methodArgs = arrayOfNulls<Any>(form.size - 3)
        /* Add rest args (if any) */
        methodArgs.indices.forEach { methodArgs[it] = evaluator.eval(form[it + 3]) }
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
