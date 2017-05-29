package core.procedures.continuations

import core.procedures.AFn
import core.procedures.FnArgs

/*
 * "Upward" one-shot continuation
 */
class Continuation : AFn(FnArgs(min = 1, max = 1)) {

    var isInvoked = false
        private set

    fun invalidate() {
        this.isInvoked = true
    }

    override val name = "continuation"

    override operator fun invoke(arg: Any?): Number? {
        throw CalledContinuation(arg!!, this)
    }

    override fun toString(): String {
        return "#<continuation>"
    }
}
