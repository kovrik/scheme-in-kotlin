package core.procedures.delayed

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Delay
import core.scm.Future
import core.scm.Promise

class Force : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val name: String
        get() = "force"

    override fun apply1(arg: Any?): Any? {
        if (arg is Future || arg is Promise) {
            return arg
        }
        /* Force derefs delays */
        if (arg is Delay) {
            return arg.deref()
        }
        throw WrongTypeException(name, "Delay or Promise or Future", arg)
    }
}
