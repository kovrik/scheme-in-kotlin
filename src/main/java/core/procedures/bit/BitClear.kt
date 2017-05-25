package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitClear : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-clear"

    override operator fun invoke(arg1: Any?, arg2: Any?): Long {
        val number = (arg1 as Number).toLong()
        return number and (1L shl (arg2 as Number).toInt()).inv()
    }
}
