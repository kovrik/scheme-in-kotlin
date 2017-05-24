package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitAndNot : AFn(FnArgsBuilder().min(2).rest(Type.BitOp::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-and-not"

    override fun apply(vararg args: Any?): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result and (args[i] as Number).toLong().inv()
        }
        return result
    }
}
