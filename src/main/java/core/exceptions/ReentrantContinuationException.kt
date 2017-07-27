package core.exceptions

class ReentrantContinuationException : RuntimeException("implementation restriction - continuation can only be used once") {

    @Synchronized override fun fillInStackTrace() = null
}
