package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

open class BitXor : AFn(name = "bit-xor", isPure = true, minArgs = 2, restArgsType = Type.BitOp::class.java) {

    override operator fun invoke(vararg args: Any?): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result xor (args[i] as Number).toLong()
        }
        return result
    }
}
