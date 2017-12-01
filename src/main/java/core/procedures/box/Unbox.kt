package core.procedures.box

import core.procedures.AFn
import core.scm.Box

class Unbox : AFn<Box<*>?, Any?>(name = "unbox", minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf(Box::class.java)) {

    override operator fun invoke(arg: Box<*>?) = arg!!.get()
}