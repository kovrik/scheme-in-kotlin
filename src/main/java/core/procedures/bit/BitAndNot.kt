package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

class BitAndNot : AFn(name = "bit-and-not", isPure = true, minArgs = 2, restArgsType = Type.BitOp::class.java) {

    override operator fun invoke(vararg args: Any?): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result and (args[i] as Number).toLong().inv()
        }
        return result
    }
}
