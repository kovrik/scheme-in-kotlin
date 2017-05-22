package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitNot : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.BitOp::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-not"

    override fun apply1(arg: Any?): Long? {
        return (arg as Number).toLong().inv()
    }
}
