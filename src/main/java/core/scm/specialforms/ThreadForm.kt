package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.IFn
import core.scm.Symbol
import core.Writer
import kotlin.concurrent.thread

/**
 * Syntax:
 * (thread <expression>)
 */
object ThreadForm : SpecialForm("thread") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Thread {
        if (form.size != 2) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val body = evaluator.eval(form[1])
        val runnable: () -> Unit = when (body) {
            is IFn<*, *> -> { -> evaluator.eval(listOf(body)) }
            else         -> throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        return when (form[1]) {
            is Symbol -> thread(name = form[1].toString(), block = runnable, start = true)
            else      -> thread(block = runnable, start = true)
        }
    }
}