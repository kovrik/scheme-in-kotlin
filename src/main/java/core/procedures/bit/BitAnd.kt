package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitAnd : AFn(FnArgsBuilder().min(2).rest(Type.BitOp::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-and"

    override fun apply(args: Array<Any?>): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result and (args[i] as Number).toLong()
        }
        return result
    }
}
