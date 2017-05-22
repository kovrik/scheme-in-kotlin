package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitSet : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-set"

    override fun apply2(arg1: Any?, arg2: Any?): Long {
        val number = (arg1 as Number).toLong()
        return number or (1L shl (arg2 as Number).toInt())
    }
}
