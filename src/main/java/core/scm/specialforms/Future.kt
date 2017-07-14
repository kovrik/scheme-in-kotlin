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
        val future: core.scm.Future
        if (form.size > 2) {
            val list: MutableList<Any?> = Cons.list(Begin)
            list.addAll(form.subList(1, form.size))
            future = core.scm.Future(list, env, evaluator)
        } else {
            future = core.scm.Future(form[1], env, evaluator)
        }
        Evaluator.executor.submit(future)
        return future
    }

    override fun toString() = "future"
}
