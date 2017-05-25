package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Thunk

class Eval : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = false

    override val name: String
        get() = "eval"

    override operator fun invoke(arg: Any?): Any {
        return Thunk(arg!!)
    }
}
