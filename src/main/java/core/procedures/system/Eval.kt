package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Thunk

class Eval : AFn(FnArgs(min = 1, max = 1)) {

    override val name = "eval"
    override operator fun invoke(arg: Any?) = Thunk(arg!!)
}
