package core.procedures.delayed

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Promise

class PromiseProc : AFn(FnArgs(max = 0)) {

    override val name = "promise"
    override operator fun invoke() = Promise()
}
