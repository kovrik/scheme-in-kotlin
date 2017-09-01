package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons

/* Syntax:
 * (lazy <expression>)
 */
object Lazy : SpecialForm("lazy") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), form)
        2    -> core.scm.Lazy(form[1], env, evaluator)
        else -> core.scm.Lazy(Cons.list<Any?>(Begin).apply { addAll(form.subList(1, form.size))}, env, evaluator)
    }
}