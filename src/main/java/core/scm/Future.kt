package core.scm

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.ThrowableWrapper
import core.writer.Writer
import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.FutureTask

open class Future(expr: Any?, env: Environment, evaluator: Evaluator) :
        FutureTask<Any?>(Callable { evaluator.eval(expr, env) }), IDeref {

    override fun deref() = try {
        get()
    } catch (e: InterruptedException) {
        when {
            e.cause is RuntimeException -> throw e.cause as RuntimeException
            else -> throw RuntimeException(e.message)
        }
    } catch (e: ExecutionException) {
        when {
            e.cause is RuntimeException -> throw e.cause as RuntimeException
            else -> throw RuntimeException(e.message)
        }
    }

    override fun toString() = StringBuilder("#<future").apply {
        when {
            isDone -> {
                append("!")
                val value = try {
                    deref()
                } catch (e: RuntimeException) {
                    append("error!")
                    (e as? ThrowableWrapper)?.cause ?: e
                }
                append(if (value == this) "(this future)" else Writer.write(value)).append('>')
            }
            isCancelled -> append(":cancelled>")
            else        -> append(":pending>")
        }
    }.toString()
}

