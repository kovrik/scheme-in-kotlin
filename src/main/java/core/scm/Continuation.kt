package core.scm

import core.procedures.AFn
import core.procedures.Arity.Exactly

/*
 * "Upward" one-shot continuation
 */
class Continuation : AFn<Any?, Unit>(name = "continuation", arity = Exactly(1)) {

    var isInvoked = false
        internal set

    override operator fun invoke(arg: Any?) = throw CalledContinuation(arg!!, this)

    override fun toString() = "#<continuation>"
}
