package core.procedures.bit

import core.procedures.AFn
import core.scm.Type
import core.utils.Utils

class ArithmeticShift : AFn(name = "arithmetic-shift", isPure = true, minArgs = 2, maxArgs = 2,
                            mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    // TODO shifts for big numbers
    override operator fun invoke(arg1: Any?, arg2: Any?): Long {
        return if (Utils.isPositive(arg2)) {
            (arg1 as Number).toLong() shl (arg2 as Number).toInt()
        } else {
            (arg1 as Number).toLong() ushr (arg2 as Number).toInt().times(-1)
        }
    }
}
