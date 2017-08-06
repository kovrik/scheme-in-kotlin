package core.scm

import core.writer.Writer

import java.util.concurrent.CompletableFuture

class Promise : CompletableFuture<Any>(), IDeref {

    override fun deref(): Any? = get()

    override fun toString() = StringBuilder("#<promise").apply {
        when {
            isCompletedExceptionally -> append("!error!>")
            isDone -> {
                val value = deref()
                append('!').append(if (value === this) "(this promise)" else Writer.write(value)).append('>')
            }
            isCancelled -> append(":cancelled>")
            else -> append(":pending>")
        }
    }.toString()
}
