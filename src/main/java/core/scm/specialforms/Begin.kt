package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.scm.Thunk

/* Syntax:
 * (begin <expression1> <expression2> ...)
 *
 * NB:
 * Begin IGNORES first element of expression!
 *
 * In most cases it will be `begin` itself:
 *   (begin e1 e2 e3 ...)  ; begin is ignored
 *
 * But we also re-use Begin from some other Special Forms (do, cond, case).
 * In these cases we ignore first element as well:
 *   (else e1 e2 e3 ...)             ; else is ignored
 *   ((1 4 6 8 9) (quote composite)) ; first list is ignored - it was checked in `case` and evaluated to #t,
 *                                   ; so here we want to evaluate the rest of the form
 *
 * It may be more clear if we replace first elements with `begin` explicitly (in case, cond and do),
 * but it is ignored anyway, so why do one extra operation?
 */
object Begin : SpecialForm("begin") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? = when {
        form.size <= 1 -> Unit
        else -> {
            for (i in 1..form.size - 2) {
                evaluator.eval(form[i], env)
            }
            Thunk(form[form.size - 1], env)
        }
    }
}
