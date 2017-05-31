package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.equivalence.Eqv
import core.scm.Void

/* Syntax:
 * (case <key> <clause1> <clause2> ...)
 *
 * <clause>: ((<datum1> ...) <expression1> <expression2> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
enum class Case : ISpecialForm {
    CASE;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        /* Save string representation of expression before evaluation */
        val exprString = expression.toString()
        if (expression.size <= 1) {
            throw IllegalSyntaxException.of(toString(), exprString, "source expression failed to match any pattern")
        }
        val key = evaluator.eval(expression[1], env)
        for (i in 2..expression.size - 1) {
            val subform = expression[i] as? List<*> ?: throw IllegalSyntaxException.of(toString(), exprString, "invalid clause in subform")
            val datum = subform[0]
            if (Else.ELSE_SYMBOL == datum) {
                if (i != expression.size - 1) {
                    throw IllegalSyntaxException.of(toString(), exprString, "else must be the last clause in subform")
                }
                return Begin.BEGIN.eval(subform, env, evaluator)
            }
            if (datum !is List<*>) {
                throw IllegalSyntaxException.of(toString(), exprString, "invalid clause in subform")
            }
            for (n in datum) {
                if (Eqv.eqv(key, n)) {
                    return Begin.BEGIN.eval(subform, env, evaluator)
                }
            }
        }
        return Void
    }

    override fun toString() = "case"
}
