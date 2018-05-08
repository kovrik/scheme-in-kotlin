package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.Writer

/* Syntax:
 * (locking <lock> <body> ...)
 */
object Locking : SpecialForm("locking") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? = when {
        form.size <= 1 -> throw IllegalSyntaxException(toString(), Writer.write(form))
        else -> synchronized(evaluator.eval(form[1], env)!!) {
            for (i in 1 until form.size - 1) { evaluator.eval(form[i], env) }
            evaluator.eval(form[form.size - 1], env)
        }
    }
}