package core.procedures.delayed

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Promise

import java.util.concurrent.CompletableFuture

class PromiseProc : AFn(FnArgs(max = 0)) {

    override val name = "promise"

    override operator fun invoke(): CompletableFuture<Any> {
        return Promise()
    }
}
