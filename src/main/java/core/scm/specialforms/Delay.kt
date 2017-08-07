package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons

/* Syntax:
 * (delay <expression>)
 */
object Delay : SpecialForm("delay") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when {
        form.size < 2  -> throw IllegalSyntaxException(toString(), form)
        form.size == 2 -> core.scm.Delay(form[1], env, evaluator)
        else -> Cons.list<Any?>(Begin).let {
            it.addAll(form.subList(1, form.size))
            core.scm.Delay(it, env, evaluator)
        }
    }
}
