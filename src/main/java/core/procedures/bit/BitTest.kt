package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitTest : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-test"

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        val n = (arg2 as Number).toInt()
        return ((arg1 as Number).toInt() shr n and 1) == 1
    }
}
