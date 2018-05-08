package core.scm

import core.environment.Environment
import core.Evaluator
import core.Writer
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

open class Future(expr: Any?, env: Environment, evaluator: Evaluator) :
        FutureTask<Any?>({ evaluator.eval(expr, env) }), IDeref {

    override fun deref() = get()

    override fun deref(timeout: Long, timeoutVal: Any?): Any? = try {
        get(timeout, TimeUnit.MILLISECONDS)
    } catch (e: TimeoutException) {
        timeoutVal
    }

    override fun toString() = when {
        isDone -> StringBuilder("#<future!").apply {
            try {
                deref().let { append(if (it == this) "(this future)" else Writer.write(it)) }
            } catch (e: Throwable) {
                append("error!").append(e)
            }
        }.append('>').toString()
        isCancelled -> "#<future:cancelled>"
        else        -> "#<future:pending>"
    }
}

