package core.procedures.delayed

import core.procedures.AFn
import core.scm.Promise

class PromiseProc : AFn(name = "promise", maxArgs = 0) {

    override operator fun invoke() = Promise()
}
