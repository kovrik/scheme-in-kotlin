package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.utils.Utils

/* Syntax:
 * (cond <clause1> <clause2> ...)
 *
 * <clause>: (<test> <expression1> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
object Cond : SpecialForm("cond") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        for (i in 1 until form.size) {
            val subform = form[i] as? List<*> ?: throw IllegalSyntaxException(toString(), form, "invalid clause in subform")
            val clause = subform[0]
            if (Else.symbol == clause) {
                if (i != form.size - 1) {
                    throw IllegalSyntaxException(toString(), form, "else must be the last clause in subform")
                }
                return Begin.eval(subform, env, evaluator)
            }
            if (Utils.toBoolean(evaluator.eval(clause, env))) {
                return Begin.eval(subform, env, evaluator)
            }
        }
        return Unit
    }
}
