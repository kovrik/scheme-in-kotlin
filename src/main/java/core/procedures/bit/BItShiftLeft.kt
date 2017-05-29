package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type

class BItShiftLeft : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType))) {

    override val isPure = true
    override val name = "bit-shift-left"

    override operator fun invoke(arg1: Any?, arg2: Any?): Long {
        return (arg1 as Number).toLong() shl (arg2 as Number).toInt()
    }
}
