package core.scm

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.ReentrantDelayException
import core.writer.Writer

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.atomic.AtomicBoolean

open class Delay(private val expr: Any?, private val env: Environment, private val evaluator: Evaluator) :
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
            complete(evaluator.eval(expr, env))
            get()
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
        isDone       -> StringBuilder("#<delay!").append(Writer.write(if (value === this) "(this delay)" else value)).toString()
        isCancelled  -> "#<delay:cancelled>"
        forced.get() -> "#<delay:running>"
        else         -> "#<delay:pending>"
    }
}
