package core.procedures.box

import core.procedures.AFn
import core.scm.Box

open class BoxProc : AFn<Any?, Box<*>>(name = "box", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = Box(arg)
}