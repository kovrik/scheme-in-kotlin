package core.exceptions

class ThrowableWrapper(private val throwable: Throwable) : RuntimeException(throwable) {

    fun get() = throwable

    @Synchronized override fun fillInStackTrace() = null
}
