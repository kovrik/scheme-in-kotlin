package core.procedures.delayed

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.IDeref

class Deref : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val name: String
        get() = "deref"

    override fun apply1(arg: Any?): Any? {
        if (arg !is IDeref) {
            throw WrongTypeException(name, "Delay or Promise or Future", arg)
        }
        return arg.deref()
    }
}
