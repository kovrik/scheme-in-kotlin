package core.procedures.delayed

import core.procedures.AFn
import core.scm.Promise

import java.util.concurrent.CompletableFuture

class Deliver : AFn(name = "deliver", minArgs = 2, maxArgs = 2, mandatoryArgsTypes = arrayOf(Promise::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): CompletableFuture<Any>? {
        val p = arg1 as Promise?
        if (p!!.isDone || p.isCompletedExceptionally) {
            return null
        }
        p.complete(arg2)
        return p
    }
}
