package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type

class BitSet : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType))) {

    override val isPure = true
    override val name = "bit-set"

    override operator fun invoke(arg1: Any?, arg2: Any?): Long {
        val number = (arg1 as Number).toLong()
        return number or (1L shl (arg2 as Number).toInt())
    }
}
