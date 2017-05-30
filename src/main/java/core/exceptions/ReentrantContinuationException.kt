package core.exceptions

class ReentrantContinuationException : RuntimeException("Re-entrant continuation: implementation restriction: continuation can only be used once") {

    @Synchronized override fun fillInStackTrace() = null
}
