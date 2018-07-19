package core.scm

import core.procedures.AFn
import core.procedures.Arity.Exactly

/*
 * "Upward" one-shot continuation
 */
class Continuation(var invoked: Boolean = false) : AFn<Any?, Unit>(name = "continuation", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = throw CalledContinuation(arg!!, this)

    override fun toString() = "#<continuation>"
}
