package core.scm

import core.Evaluator
import core.exceptions.ReentrantDelayException
import java.util.concurrent.atomic.AtomicBoolean

class Lazy(private val expr: Any?, private val evaluator: Evaluator) : Delay(expr, evaluator) {

    private val forced = AtomicBoolean(false)

    override fun deref(): Any? = when {
        isCancelled -> null
        isCompletedExceptionally || isDone -> value
        /* Do not allow delay to be forced twice */
        !forced.compareAndSet(false, true) -> throw ReentrantDelayException(this)
        else -> try {
            /* Always run delay in the current thread */
            val value = evaluator.eval(expr).let {
                /* If result is a Delay, then force it */
                when (it) {
                    is Delay -> it.deref()
                    else -> it
                }
            }
            complete(value)
            get()
        } catch (e: Throwable) {
            completeExceptionally(e)
            value
        }
    }
}