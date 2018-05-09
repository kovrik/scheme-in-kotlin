package core.scm

import core.Writer

import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

class Promise : CompletableFuture<Any>(), IDeref {

    override fun deref(): Any? = get()

    override fun deref(timeout: Long, timeoutVal: Any?): Any? = try {
        get(timeout, TimeUnit.MILLISECONDS)
    } catch (e: TimeoutException) {
        timeoutVal
    }

    override fun toString() = when {
        isDone                   -> "#<promise!${Writer.write(deref())}>"
        isCompletedExceptionally -> "#<promise!error!>"
        isCancelled              -> "#<promise:cancelled>"
        else                     -> "#<promise:pending>"
    }
}
