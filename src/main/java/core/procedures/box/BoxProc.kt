package core.procedures.box

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Box

open class BoxProc : AFn<Any?, Box<*>>(name = "box", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Box(arg)
}