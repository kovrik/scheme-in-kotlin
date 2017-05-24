package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons

/* Syntax:
 * (delay <expression>)
 */
enum class Delay : ISpecialForm {
    DELAY;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): core.scm.Delay {
        if (expression.size < 2) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        val expr: Any?
        if (expression.size > 2) {
            val list: MutableList<Any?> = Cons.list(Begin.BEGIN)
            list.addAll(expression.subList(1, expression.size))
            expr = list
        } else {
            expr = expression[1]
        }
        return core.scm.Delay(expr, env, evaluator)
    }

    override fun toString(): String {
        return "delay"
    }
}
