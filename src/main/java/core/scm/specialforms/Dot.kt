package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.Reflector
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol
import core.Writer

object Dot : SpecialForm(".") {

    private val reflector = Reflector()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = form.size
        if (size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form), "has ${size - 1} parts after keyword")
        }
        // FIXME Optimize and cleanup
        val first = if (form[1] is Symbol) {
            env.findOrDefault(form[1], evaluator.eval(form[1], env))
        } else {
            evaluator.eval(form[1], env)
        }
        if (first is Class<*>) {
            val statik = "${form[1]}/${form[2]}"
            if (form.size == 3) {
                /* (. Classname-symbol member-symbol) */
                /* try static field first */
                return try {
                    reflector.evalJavaStaticField(statik)
                } catch (e: NoSuchFieldException) {
                    /* now try static no-args static method */
                    reflector.evalJavaMethod(statik, arrayOf())
                }
            } else {
                /* (. Classname-symbol method-symbol args) */
                val methodArgs = arrayOfNulls<Any>(form.size - 3)
                /* Add args */
                for (i in methodArgs.indices) {
                    methodArgs[i] = evaluator.eval(form[i + 3], env)
                }
                return reflector.evalJavaMethod(statik, methodArgs)
            }
        } else {
            /* (. instance-expr member-symbol)
             * (. instance-expr -field-symbol)
             * (. instance-expr method-symbol args)
             */
            val methodArgs = arrayOfNulls<Any>(form.size - 2)
            /* Add instance */
            methodArgs[0] = first
            /* Add rest args (if any) */
            for (i in 1 until methodArgs.size) {
                methodArgs[i] = evaluator.eval(form[i + 2], env)
            }
            val method = '.' + form[2].toString()
            return reflector.evalJavaMethod(method, methodArgs)
        }
    }
}
