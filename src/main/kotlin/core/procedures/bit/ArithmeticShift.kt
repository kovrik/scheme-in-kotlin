package core.procedures.bit

import core.procedures.AFn
import core.scm.Type
import core.utils.Utils

class ArithmeticShift : AFn<Number?, Long>(name = "arithmetic-shift", isPure = true, minArgs = 2, maxArgs = 2,
                            mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    // TODO shifts for big numbers
    override operator fun invoke(arg1: Number?, arg2: Number?) = when {
        Utils.isPositive(arg2!!) -> arg1!!.toLong() shl arg2.toInt()
        else -> arg1!!.toLong() ushr arg2.toInt().times(-1)
    }
}
