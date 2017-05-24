package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitXor : AFn(FnArgsBuilder().min(2).rest(Type.BitOp::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bit-xor"

    override fun apply(vararg args: Any?): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result xor (args[i] as Number).toLong()
        }
        return result
    }
}
