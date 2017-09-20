package core.procedures.delayed

import core.procedures.AFn
import core.scm.Delay

class Force : AFn<Any?, Any?>(name = "force", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Delay -> arg.deref()
        else     -> arg
    }
}
