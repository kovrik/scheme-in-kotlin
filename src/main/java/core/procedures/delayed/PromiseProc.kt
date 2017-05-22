package core.procedures.delayed

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Promise

import java.util.concurrent.CompletableFuture

class PromiseProc : AFn(FnArgsBuilder().max(0).build()) {

    override val name: String
        get() = "promise"

    override fun apply0(): CompletableFuture<Any> {
        return Promise()
    }
}
