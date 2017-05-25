package core.procedures.continuations

import core.procedures.AFn
import core.procedures.FnArgsBuilder

/*
 * "Upward" one-shot continuation
 */
class Continuation : AFn(FnArgsBuilder().min(1).max(1).build()) {

    var isInvoked = false
        private set

    fun invalidate() {
        this.isInvoked = true
    }

    override val isPure: Boolean
        get() = false

    override val name: String
        get() = "continuation"

    override operator fun invoke(arg: Any?): Number? {
        throw CalledContinuation(arg!!, this)
    }

    override fun toString(): String {
        return "#<continuation>"
    }
}
