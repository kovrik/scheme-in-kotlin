package core.procedures.box

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.WeakBox

class WeakBoxValue : AFn<WeakBox<*>?, Any?>(name = "weak-box-value", arity = Exactly(1),
                                            mandatoryArgsTypes = arrayOf(WeakBox::class.java)) {

    override operator fun invoke(arg: WeakBox<*>?) = arg!!.get()
}