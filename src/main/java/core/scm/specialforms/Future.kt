package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons

/* Syntax:
 * (future <expression>)
 */
object Future : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 2) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        return if (form.size > 2) {
            val list = Cons.list<Any?>(Begin)
            list.addAll(form.subList(1, form.size))
            core.scm.Future(list, env, evaluator).apply { Evaluator.executor.submit(this) }
        } else {
            core.scm.Future(form[1], env, evaluator).apply { Evaluator.executor.submit(this) }
        }
    }

    override fun toString() = "future"
}
