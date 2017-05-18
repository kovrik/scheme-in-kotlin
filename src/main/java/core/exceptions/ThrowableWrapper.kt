package core.exceptions

class ThrowableWrapper(private val throwable: Throwable) : RuntimeException(throwable) {

    fun get(): Throwable {
        return throwable
    }

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }
}
