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

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): core.scm.Delay {
        if (form.size < 2) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        val expr: Any?
        if (form.size > 2) {
            val list: MutableList<Any?> = Cons.list(Begin.BEGIN)
            list.addAll(form.subList(1, form.size))
            expr = list
        } else {
            expr = form[1]
        }
        return core.scm.Delay(expr, env, evaluator)
    }

    override fun toString() = "delay"
}
