package core.procedures.bit

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

open class BitNot : AFn<Number?, Long>(name = "bit-not", isPure = true, arity = Exactly(1),
                                       mandatoryArgsTypes = arrayOf(Type.BitOp::class.java)) {

    override operator fun invoke(arg: Number?) = arg!!.toLong().inv()
}
