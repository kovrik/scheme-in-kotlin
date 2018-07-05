package core.procedures.delayed

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Delay

class Force : AFn<Any?, Any?>(name = "force", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Delay -> arg.deref()
        else     -> arg
    }
}
