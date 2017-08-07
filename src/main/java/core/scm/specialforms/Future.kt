package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons

/* Syntax:
 * (future <expression>)
 */
object Future : SpecialForm("future") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), form)
        2    -> form[1]
        else -> Cons.list<Any?>(Begin).apply { addAll(form.subList(1, form.size)) }
    }.let { core.scm.Future(it, env, evaluator).apply { Evaluator.executor.submit(this) } }
}
