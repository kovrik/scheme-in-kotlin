package core.procedures.bit

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

class BitShiftRight : AFn<Number?, Long>(name = "bit-shift-right", isPure = true, arity = Exactly(2),
                                         mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?) = arg1!!.toLong() shr arg2!!.toInt()
}
