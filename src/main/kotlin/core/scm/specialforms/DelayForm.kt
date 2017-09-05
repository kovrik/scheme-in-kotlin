package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Delay

/* Syntax:
 * (delay <expression>)
 */
object DelayForm : SpecialForm("delay") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), form)
        2    -> Delay(form[1], env, evaluator)
        else -> Delay(Cons.list<Any?>(Begin).apply { addAll(form.subList(1, form.size))}, env, evaluator)
    }
}
