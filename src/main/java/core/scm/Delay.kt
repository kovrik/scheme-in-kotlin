package core.scm

import core.Evaluator
import core.exceptions.ReentrantDelayException
import core.Writer

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean

open class Delay(private val expr: Any?, private val evaluator: Evaluator) :
           CompletableFuture<Any>(), IDeref {

    private val forced = AtomicBoolean(false)

    fun isForced() = forced.get()

    override fun deref(): Any? = when {
        isCancelled -> null
        isCompletedExceptionally || isDone -> value
        /* Do not allow delay to be forced twice */
        !forced.compareAndSet(false, true) -> throw ReentrantDelayException(this)
        else -> try {
            /* Always run delay in the current thread */
            complete(evaluator.eval(expr))
            get()
        } catch (e: Throwable) {
            completeExceptionally(e)
            value
        }
    }

    override fun deref(timeout: Long, timeoutVal: Any?): Any? = when {
        isCancelled -> null
        isCompletedExceptionally || isDone -> value
        /* Do not allow delay to be forced twice */
        !forced.compareAndSet(false, true) -> throw ReentrantDelayException(this)
        else -> try {
            /* Always run delay in the current thread */
            complete(evaluator.eval(expr))
            get(timeout, TimeUnit.MILLISECONDS)
        } catch (e: TimeoutException) {
            timeoutVal
        } catch (e: Throwable) {
            completeExceptionally(e)
            value
        }
    }

    internal val value: Any?
        get() = try {
            get()
        } catch (e: InterruptedException) {
            throw e.cause!!
        } catch (e: ExecutionException) {
            throw e.cause!!
        }

    override fun toString() = when {
        isCompletedExceptionally -> StringBuilder("#<delay!error!").apply {
            val value = try {
                get()
            } catch (e: Throwable) {
                e
            }
            append(if (value === this) "(this delay)" else Writer.write(value)).append('>')
        }.toString()
        isCancelled  -> "#<delay:cancelled>"
        isDone       -> StringBuilder("#<delay!").append(if (value === this) "(this delay)" else Writer.write(value)).append('>').toString()
        forced.get() -> "#<delay:running>"
        else         -> "#<delay:pending>"
    }
}
