package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.evaluator.Reflector
import core.exceptions.IllegalSyntaxException

enum class New : ISpecialForm {
    NEW;

    private val reflector = Reflector()

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (expression.size < 2) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        val clazz = expression[1].toString()
        val args = arrayOfNulls<Any>(expression.size - 2)
        for (i in args.indices) {
            args[i] = evaluator.eval(expression[i + 2], env)
        }
        return reflector.newInstance(clazz, args)
    }

    override fun toString() = "new"
}
