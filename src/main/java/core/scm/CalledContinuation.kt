package core.scm

class CalledContinuation internal constructor(val value: Any, val continuation: Continuation) : RuntimeException("CalledContinuationException") {

    /* Do not fill in the execution stack trace (we don't need it anyway) to make CalledContinuations much faster */
    @Synchronized override fun fillInStackTrace() = null

    override fun toString() = "CalledContinuation{value=$value, continuation=$continuation}"
}
