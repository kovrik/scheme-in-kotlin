package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class BitAndNot : AFn(FnArgsBuilder().min(2).rest(Type.BitOp::class.java).build()) {

    override val isPure = true
    override val name = "bit-and-not"

    override operator fun invoke(vararg args: Any?): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result and (args[i] as Number).toLong().inv()
        }
        return result
    }
}
