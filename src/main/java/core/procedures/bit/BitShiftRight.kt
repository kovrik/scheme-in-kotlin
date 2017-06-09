package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

class BitShiftRight : AFn(name = "bit-shift-right", isPure = true, minArgs = 2, maxArgs = 2,
                          mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Long {
        return (arg1 as Number).toLong() shr (arg2 as Number).toInt()
    }
}
