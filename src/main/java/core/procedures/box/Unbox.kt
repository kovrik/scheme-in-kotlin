package core.procedures.box

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Box

class Unbox : AFn<Box<*>?, Any?>(name = "unbox", arity = Exactly(1),
                                 mandatoryArgsTypes = arrayOf(Box::class.java)) {

    override operator fun invoke(arg: Box<*>?) = arg!!.get()
}