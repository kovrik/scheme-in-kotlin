package core.procedures.system

import core.procedures.AFn
import core.scm.Thunk

class Eval : AFn<Any?, Any?>(name = "eval", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = Thunk(arg!!)
}
