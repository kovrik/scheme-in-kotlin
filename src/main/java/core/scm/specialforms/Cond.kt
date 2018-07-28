package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.utils.Utils
import core.Writer

/* Syntax:
 * (cond <clause1> <clause2> ...)
 *
 * <clause>: (<test> <expression1> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
object Cond : SpecialForm("cond") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? = (1 until form.size).forEach {
        val subform = form[it] as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form), "invalid clause in subform")
        val clause = subform[0]
        when {
            Else.symbol == clause -> {
                if (it != form.size - 1) {
                    throw IllegalSyntaxException(toString(), Writer.write(form), "else must be the last clause in subform")
                }
                return Begin.eval(subform, env, evaluator)
            }
            Utils.toBoolean(evaluator.eval(clause, env)) -> return Begin.eval(subform, env, evaluator)
        }
    }
}
