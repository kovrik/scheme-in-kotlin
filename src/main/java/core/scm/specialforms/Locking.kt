package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.Writer

/**
 * Syntax:
 * (locking <lock> <body> ...)
 */
object Locking : SpecialForm("locking") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? = when {
        form.size <= 1 -> throw IllegalSyntaxException(toString(), Writer.write(form))
        else -> synchronized(evaluator.eval(form[1])!!) {
            (1 until form.size - 1).forEach { evaluator.eval(form[it]) }
            evaluator.eval(form[form.size - 1])
        }
    }
}