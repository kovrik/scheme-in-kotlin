package core.scm

import core.writer.Writer

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException

class Promise : CompletableFuture<Any>(), IDeref {

    override fun deref(): Any? {
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

    override fun toString(): String {
        val sb = StringBuilder("#<promise")
        when {
            isCompletedExceptionally -> sb.append("!error!")
            isDone -> {
                val value = deref()
                sb.append("!").append(if (value === this) "(this promise)" else Writer.write(value))
            }
            isCancelled -> sb.append(":cancelled")
            else -> sb.append(":pending")
        }
        return sb.append(">").toString()
    }
}
