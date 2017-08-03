package core.exceptions

class ThrowableWrapper(private val throwable: Throwable) : RuntimeException(throwable) {

    override val cause: Throwable
        get() = throwable

    @Synchronized override fun fillInStackTrace() = null
}
