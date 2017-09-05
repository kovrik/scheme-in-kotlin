package core.scm

import core.environment.Environment
import core.evaluator.Evaluator
import core.writer.Writer
import java.util.concurrent.FutureTask

open class Future(expr: Any?, env: Environment, evaluator: Evaluator) :
        FutureTask<Any?>({ evaluator.eval(expr, env) }), IDeref {

    override fun deref() = get()

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

