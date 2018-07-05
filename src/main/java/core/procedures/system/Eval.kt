package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Thunk

class Eval : AFn<Any?, Any?>(name = "eval", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Thunk(arg)
}
