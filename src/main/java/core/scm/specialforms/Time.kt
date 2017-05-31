package core.scm.specialforms

import core.Repl
import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.ThrowableWrapper

import java.io.IOException

/**
 * Time Special Form:
 * Counts time taken for evaluation.
 * Syntax:
 * (time <expression1> ... <expression n>)
 */
enum class Time : ISpecialForm {
    TIME;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (expression.size < 2) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        val start = System.nanoTime()
        for (i in 1..expression.size - 1 - 1) {
            evaluator.eval(expression[i], env)
        }
        val result = evaluator.eval(expression[expression.size - 1], env)
        val diff = (System.nanoTime() - start).toDouble() / 1000000
        try {
            Repl.currentOutputPort.writeln("elapsed time: $diff ms")
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
        return result
    }

    override fun toString() = "time"
}
