package core.procedures.box

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.WeakBox

open class MakeWeakBox : AFn<Any?, WeakBox<*>>(name = "make-weak-box", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = WeakBox(arg)
}