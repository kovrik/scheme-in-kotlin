package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

class BitSet : AFn<Number?, Long>(name = "bit-set", isPure = true, minArgs = 2, maxArgs = 2,
                                  mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?) = arg1!!.toLong() or (1L shl arg2!!.toInt())
}
