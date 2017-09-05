package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Lazy

/* Syntax:
 * (lazy <expression>)
 */
object LazyForm : SpecialForm("lazy") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        1    -> throw IllegalSyntaxException(toString(), form)
        2    -> Lazy(form[1], env, evaluator)
        else -> Lazy(Cons.list<Any?>(Begin).apply { addAll(form.subList(1, form.size))}, env, evaluator)
    }
}