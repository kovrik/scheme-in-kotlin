package core.scm

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.ReentrantDelayException
import core.writer.Writer

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.atomic.AtomicBoolean

class Delay(private val expr: Any?, private val env: Environment, private val evaluator: Evaluator) : CompletableFuture<Any>(), IDeref {

    private val forced = AtomicBoolean(false)

    override fun deref(): Any? {
        if (isCancelled) {
            return null
        }
        if (isCompletedExceptionally || isDone) {
            return value
        }
        if (!forced.compareAndSet(false, true)) {
            /* Do not allow delay to be forced twice */
            throw ReentrantDelayException(this)
        }
        try {
            /* Always run delay in current thread */
            complete(evaluator.eval(expr, env))
            return get()
        } catch (e: Exception) {
            completeExceptionally(e)
        }

        return value
    }

    private val value: Any
        get() {
            try {
                return get()
            } catch (e: InterruptedException) {
                if (e.cause is RuntimeException) {
                    throw e.cause as RuntimeException
                }
                throw RuntimeException(e.message)
            } catch (e: ExecutionException) {
                if (e.cause is RuntimeException) {
                    throw e.cause as RuntimeException
                }
                throw RuntimeException(e.message)
            }

        }

    override fun toString(): String {
        val sb = StringBuilder("#<").append("delay")
        if (isCompletedExceptionally) {
            var value: Any
            try {
                value = get()
            } catch (e: RuntimeException) {
                value = e
            }

            sb.append("!error!").append(if (value === this) "(this delay)" else Writer.write(value))
        } else if (isDone) {
            val value = value
            sb.append("!").append(if (value === this) Writer.write("(this delay)") else Writer.write(value))
        } else if (isCancelled) {
            sb.append(":cancelled")
        } else if (forced.get()) {
            sb.append(":running")
        } else {
            sb.append(":pending")
        }
        return sb.append(">").toString()
    }
}
