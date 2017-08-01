package core.scm

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.ReentrantDelayException
import core.exceptions.ThrowableWrapper
import core.writer.Writer

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.atomic.AtomicBoolean

class Delay(private val expr: Any?, private val env: Environment, private val evaluator: Evaluator) : CompletableFuture<Any>(), IDeref {

    private val forced = AtomicBoolean(false)

    override fun deref(): Any? {
        when {
            isCancelled -> return null
            isCompletedExceptionally || isDone -> return value
            /* Do not allow delay to be forced twice */
            !forced.compareAndSet(false, true) -> throw ReentrantDelayException(this)
            else -> {
                try {
                    /* Always run delay in the current thread */
                    complete(evaluator.eval(expr, env))
                    return get()
                } catch (e: Exception) {
                    completeExceptionally(e)
                }
                return value
            }
        }
    }

    private val value: Any?
        get() {
            try {
                return get()
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
        }

    override fun toString() = StringBuilder("#<delay").apply{
        when {
            isCompletedExceptionally -> {
                val value = try {
                    get()
                } catch (e: ExecutionException) {
                    (e.cause as? ThrowableWrapper)?.get() ?: e.cause
                } catch (e: RuntimeException) {
                    (e as? ThrowableWrapper)?.get() ?: e
                }
                append("!error!").append(if (value === this) "(this delay)" else Writer.write(value)).append('>')
            }
            isDone -> append("!").append(if (value === this) Writer.write("(this delay)") else Writer.write(value))
            isCancelled  -> append(":cancelled>")
            forced.get() -> append(":running>")
            else         -> append(":pending>")
        }
    }.toString()
}
