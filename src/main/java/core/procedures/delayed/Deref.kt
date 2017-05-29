package core.procedures.delayed

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.IDeref

class Deref : AFn(FnArgs(min = 1, max = 1)) {

    override val name = "deref"

    override operator fun invoke(arg: Any?): Any? {
        if (arg !is IDeref) {
            throw WrongTypeException(name, "Delay or Promise or Future", arg)
        }
        return arg.deref()
    }
}
