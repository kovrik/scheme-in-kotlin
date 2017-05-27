package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Void
import core.utils.Utils

/* Syntax:
 * (cond <clause1> <clause2> ...)
 *
 * <clause>: (<test> <expression1> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
enum class Cond : ISpecialForm {
    COND;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        for (i in 1..expression.size - 1) {
            val subform = expression[i] as? List<*> ?: throw IllegalSyntaxException.of(toString(), expression, "invalid clause in subform")
            val clause = subform[0]
            if (Else.ELSE_SYMBOL == clause) {
                if (i != expression.size - 1) {
                    throw IllegalSyntaxException.of(toString(), expression, "else must be the last clause in subform")
                }
                return Begin.BEGIN.eval(subform, env, evaluator)
            }
            if (Utils.toBoolean(evaluator.eval(clause, env))) {
                return Begin.BEGIN.eval(subform, env, evaluator)
            }
        }
        return Void
    }

    override fun toString(): String {
        return "cond"
    }
}
