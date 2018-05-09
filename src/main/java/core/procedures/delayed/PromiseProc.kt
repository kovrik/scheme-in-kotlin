package core.procedures.delayed

import core.procedures.AFn
import core.scm.Promise

open class PromiseProc : AFn<Nothing, Promise>(name = "promise", maxArgs = 0) {

    override operator fun invoke() = Promise()
}
