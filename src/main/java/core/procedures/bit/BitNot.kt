package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitNot : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.BitOp::class.java)).build()) {

    override val isPure = true
    override val name = "bit-not"

    override operator fun invoke(arg: Any?): Long? {
        return (arg as Number).toLong().inv()
    }
}
