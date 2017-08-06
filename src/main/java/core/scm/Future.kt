package core.scm

import core.environment.Environment
import core.evaluator.Evaluator
import core.writer.Writer
import java.util.concurrent.Callable
import java.util.concurrent.FutureTask

open class Future(expr: Any?, env: Environment, evaluator: Evaluator) :
        FutureTask<Any?>(Callable { evaluator.eval(expr, env) }), IDeref {

    override fun deref() = get()

    override fun toString() = StringBuilder("#<future").apply {
        when {
            isDone -> {
                append("!")
                val value = try {
                    deref()
                } catch (e: Throwable) {
                    append("error!").append(e)
                }
                append(if (value == this) "(this future)" else Writer.write(value)).append('>')
            }
            isCancelled -> append(":cancelled>")
            else        -> append(":pending>")
        }
    }.toString()
}

