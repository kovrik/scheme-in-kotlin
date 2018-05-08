package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.IFn
import core.scm.Symbol
import core.Writer

/* Syntax:
 * (thread <expression>)
 */
object ThreadForm : SpecialForm("thread") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Thread {
        if (form.size != 2) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val body = evaluator.eval(form[1], env)
        val runnable = when (body) {
            is IFn<*, *> -> Runnable { evaluator.eval(listOf(body), env) }
            else         -> throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        return when (form[1]) {
            is Symbol -> Thread(runnable, form[1].toString()).apply { start() }
            else      -> Thread(runnable, "").apply { start() }
        }
    }
}