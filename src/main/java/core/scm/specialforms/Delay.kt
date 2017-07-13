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
        return when {
            form.size < 2  -> throw IllegalSyntaxException.of(toString(), form)
            form.size == 2 -> core.scm.Delay(form[1], env, evaluator)
            else -> {
                val list: MutableList<Any?> = Cons.list(Begin.BEGIN)
                list.addAll(form.subList(1, form.size))
                core.scm.Delay(list, env, evaluator)
            }
        }
    }

    override fun toString() = "delay"
}
