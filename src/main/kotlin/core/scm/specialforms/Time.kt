package core.scm.specialforms

import core.Repl
import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException

import java.util.concurrent.TimeUnit

/**
 * Time Special Form:
 * Counts time taken for evaluation.
 * Syntax:
 * (time <expression1> ... <expression n>)
 */
object Time : SpecialForm("time") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 2) {
            throw IllegalSyntaxException(toString(), form)
        }
        val start = System.nanoTime()
        for (i in 1..form.size - 2) {
            evaluator.eval(form[i], env)
        }
        val result = evaluator.eval(form[form.size - 1], env)
        val diff = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start)
        Repl.currentOutputPort.writeln("elapsed time: $diff ms")
        return result
    }
}
