package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitOr : AFn(FnArgsBuilder().min(2).rest(Type.BitOp::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-or"

    override fun apply(vararg args: Any?): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result or (args[i] as Number).toLong()
        }
        return result
    }
}
