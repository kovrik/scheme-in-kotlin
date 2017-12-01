package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

class BitFlip : AFn<Number?, Long>(name = "bit-flip", isPure = true, minArgs = 2, maxArgs = 2,
                                   mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?) = arg1!!.toLong() xor (1L shl arg2!!.toInt())
}
