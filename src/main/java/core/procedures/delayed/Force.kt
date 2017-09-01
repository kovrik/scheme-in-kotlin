package core.procedures.delayed

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Box
import core.scm.Delay
import core.scm.Future
import core.scm.Promise

class Force : AFn<Any?, Any?>(name = "force", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Future, is Promise, is Box<*>, is Thread -> arg
        /* Force derefs delays */
        is Delay -> arg.deref()
        else -> throw WrongTypeException(name, "Delay or Promise or Future or Box", arg)
    }
}
