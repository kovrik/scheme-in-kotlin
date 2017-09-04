package core.procedures.box

import core.procedures.AFn
import core.scm.WeakBox

open class MakeWeakBox : AFn<Any?, WeakBox<*>>(name = "make-weak-box", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = WeakBox(arg)
}