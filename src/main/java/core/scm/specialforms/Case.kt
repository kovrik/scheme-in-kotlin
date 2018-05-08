package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.equivalence.Eqv
import core.Writer

/* Syntax:
 * (case <key> <clause1> <clause2> ...)
 *
 * <clause>: ((<datum1> ...) <expression1> <expression2> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
object Case : SpecialForm("case") {

    val eqv = Eqv()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        /* Save string representation of form before evaluation */
        val exprString = Writer.write(form)
        if (form.size <= 1) {
            throw IllegalSyntaxException(toString(), exprString, "source expression failed to match any pattern")
        }
        val key = evaluator.eval(form[1], env)
        for (i in 2 until form.size) {
            val subform = form[i] as? List<*> ?: throw IllegalSyntaxException(toString(), exprString, "invalid clause in subform")
            val datum = subform[0]
            when (datum) {
                Else.symbol -> {
                    if (i != form.size - 1) {
                        throw IllegalSyntaxException(toString(), exprString, "else must be the last clause in subform")
                    }
                    return Begin.eval(subform, env, evaluator)
                }
                is List<*> -> datum.firstOrNull { eqv.eqv(key, it) }?.let { return Begin.eval(subform, env, evaluator) }
                else       -> throw IllegalSyntaxException(toString(), exprString, "invalid clause in subform")
            }
        }
        return Unit
    }
}
