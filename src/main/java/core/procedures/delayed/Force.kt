package core.procedures.delayed

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Delay
import core.scm.Future
import core.scm.Promise

class Force : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val name = "force"

    override operator fun invoke(arg: Any?): Any? {
        when (arg) {
            is Future, is Promise -> return arg
            /* Force derefs delays */
            is Delay -> return arg.deref()
            else -> throw WrongTypeException(name, "Delay or Promise or Future", arg)
        }
    }
}
