package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type

open class BitNot : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.BitOp::class.java))) {

    override val isPure = true
    override val name = "bit-not"

    override operator fun invoke(arg: Any?): Long? {
        return (arg as Number).toLong().inv()
    }
}
