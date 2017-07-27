package core.scm

import core.procedures.AFn

/*
 * "Upward" one-shot continuation
 */
class Continuation : AFn<Any?, Unit>(name = "continuation", minArgs = 1, maxArgs = 1) {

    var isInvoked = false
        private set

    fun invalidate() {
        this.isInvoked = true
    }

    override operator fun invoke(arg: Any?) = throw CalledContinuation(arg!!, this)

    override fun toString() = "#<continuation>"
}
