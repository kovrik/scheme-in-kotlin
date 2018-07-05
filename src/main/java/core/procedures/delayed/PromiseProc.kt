package core.procedures.delayed

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Promise

open class PromiseProc : AFn<Nothing, Promise>(name = "promise", arity = Exactly(0)) {

    override operator fun invoke() = Promise()
}
