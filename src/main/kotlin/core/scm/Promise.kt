package core.scm

import core.writer.Writer

import java.util.concurrent.CompletableFuture

class Promise : CompletableFuture<Any>(), IDeref {

    override fun deref(): Any? = get()

    override fun toString() = when {
        isDone -> StringBuilder("#<promise!").apply {
            val value = deref()
            append(if (value === this) "(this promise)" else Writer.write(value)).append('>')
        }.toString()
        isCompletedExceptionally -> "#<promise!error!>"
        isCancelled              -> "#<promise:cancelled>"
        else                     -> "#<promise:pending>"
    }
}
