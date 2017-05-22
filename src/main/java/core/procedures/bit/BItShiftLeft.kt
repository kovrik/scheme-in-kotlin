package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BItShiftLeft : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-shift-left"

    override fun apply2(arg1: Any?, arg2: Any?): Long {
        return (arg1 as Number).toLong() shl (arg2 as Number).toInt()
    }
}
