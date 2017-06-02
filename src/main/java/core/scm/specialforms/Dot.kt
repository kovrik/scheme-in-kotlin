package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.evaluator.Reflector
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

enum class Dot : ISpecialForm {
    DOT;

    private val reflector = Reflector()

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = expression.size
        if (size < 3) {
            throw IllegalSyntaxException.of(toString(), expression, "has ${size - 1} parts after keyword")
        }
        // FIXME Optimize and cleanup
        var first: Any? = expression[1]
        if (first is Symbol) {
            first = env.findOrDefault(first, evaluator.eval(first, env))
        } else {
            first = evaluator.eval(first, env)
        }
        if (first is Class<*>) {
            val statik = expression[1].toString() + "/" + expression[2]
            if (expression.size == 3) {
                /* (. Classname-symbol member-symbol) */
                /* try static field first */
                try {
                    return reflector.evalJavaStaticField(statik)
                } catch (e: RuntimeException) {
                    if (e.cause is NoSuchFieldException) {
                        /* now try static no-args static method */
                        return reflector.evalJavaMethod(statik, arrayOf<Any?>())
                    }
                    throw e
                }

            } else {
                /* (. Classname-symbol method-symbol args) */
                val args = arrayOfNulls<Any>(expression.size - 3)
                /* Add args */
                for (i in args.indices) {
                    args[i] = evaluator.eval(expression[i + 3], env)
                }
                return reflector.evalJavaMethod(statik, args)
            }
        } else {
            /* (. instance-expr member-symbol)
             * (. instance-expr -field-symbol)
             * (. instance-expr method-symbol args)
             */
            val method = '.' + expression[2].toString()
            val args = arrayOfNulls<Any>(expression.size - 2)
            /* Add instance */
            args[0] = first
            /* Add rest args (if any) */
            for (i in 1..args.size - 1) {
                args[i] = evaluator.eval(expression[i + 2], env)
            }
            return reflector.evalJavaMethod(method, args)
        }
    }

    override fun toString() = "."
}
