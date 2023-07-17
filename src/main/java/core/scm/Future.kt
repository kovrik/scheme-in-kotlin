package core.scm

import core.Evaluator
import core.Writer
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

open class Future(expr: Any?, evaluator: Evaluator) :
        FutureTask<Any?>({ evaluator.eval(expr) }), IDeref {

    override fun deref() = get()

    override fun deref(timeout: Long, timeoutVal: Any?): Any? = try {
        get(timeout, TimeUnit.MILLISECONDS)
    } catch (e: TimeoutException) {
        timeoutVal
    }

    override fun toString() = when {
        isCancelled -> "#<future:cancelled>"
        isDone -> StringBuilder("#<future!").apply {
            try {
                deref().let { append(if (it == this) "(this future)" else Writer.write(it)) }
            } catch (e: Throwable) {
                append("error!").append(e)
            }
        }.append('>').toString()
        else -> "#<future:pending>"
    }
}

