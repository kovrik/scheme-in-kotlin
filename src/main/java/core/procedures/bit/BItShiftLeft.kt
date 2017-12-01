package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

class BItShiftLeft : AFn<Number?, Long>(name = "bit-shift-left", isPure = true, minArgs = 2, maxArgs = 2,
                                        mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?) = arg1!!.toLong() shl arg2!!.toInt()
}
