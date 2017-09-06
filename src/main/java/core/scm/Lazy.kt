package core.scm

import core.environment.Environment
import core.Evaluator
import core.exceptions.ReentrantDelayException
import java.util.concurrent.atomic.AtomicBoolean

open class Lazy(private val expr: Any?, private val env: Environment, private val evaluator: Evaluator) :
        Delay(expr, env, evaluator) {

    private val forced = AtomicBoolean(false)

    override fun deref(): Any? = when {
        isCancelled -> null
        isCompletedExceptionally || isDone -> value
        /* Do not allow delay to be forced twice */
        !forced.compareAndSet(false, true) -> throw ReentrantDelayException(this)
        else -> try {
            /* Always run delay in the current thread */
            var value = evaluator.eval(expr, env)
            /* If result is a Delay, then force it */
            if (value is Delay) {
                value = value.deref()
            }
            complete(value)
            get()
        } catch (e: Throwable) {
            completeExceptionally(e)
            value
        }
    }
}