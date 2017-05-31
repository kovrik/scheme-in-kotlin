package core.procedures.bit

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type

open class BitAnd : AFn(FnArgs(min = 2, rest = Type.BitOp::class.java)) {

    override val isPure = true
    override val name = "bit-and"

    override operator fun invoke(vararg args: Any?): Long? {
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result and (args[i] as Number).toLong()
        }
        return result
    }
}
