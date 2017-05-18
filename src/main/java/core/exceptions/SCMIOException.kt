package core.exceptions

class SCMIOException(e: Throwable) : RuntimeException(e) {

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }
}
