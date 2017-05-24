package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons

/* Syntax:
 * (future <expression>)
 */
enum class Future : ISpecialForm {
    FUTURE;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (expression.size < 2) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        val future: core.scm.Future
        if (expression.size > 2) {
            val list: MutableList<Any?> = Cons.list(Begin.BEGIN)
            list.addAll(expression.subList(1, expression.size))
            future = core.scm.Future(list, env, evaluator)
        } else {
            future = core.scm.Future(expression[1], env, evaluator)
        }
        Evaluator.executor.submit(future)
        return future
    }

    override fun toString(): String {
        return "future"
    }
}
