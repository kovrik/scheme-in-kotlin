package core.procedures.box

import core.procedures.AFn
import core.scm.WeakBox

class WeakBoxValue : AFn<WeakBox<*>?, Any?>(name = "weak-box-value", minArgs = 1, maxArgs = 1,
                                            mandatoryArgsTypes = arrayOf<Class<*>>(WeakBox::class.java)) {

    override operator fun invoke(arg: WeakBox<*>?) = arg!!.get()
}