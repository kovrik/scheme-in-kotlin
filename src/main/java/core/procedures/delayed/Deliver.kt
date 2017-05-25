package core.procedures.delayed

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Promise

import java.util.concurrent.CompletableFuture

class Deliver : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Promise::class.java, Any::class.java)).build()) {

    override val name: String
        get() = "deliver"

    override operator fun invoke(arg1: Any?, arg2: Any?): CompletableFuture<Any>? {
        val p = arg1 as Promise?
        if (p!!.isDone || p.isCompletedExceptionally) {
            return null
        }
        p.complete(arg2)
        return p
    }
}
