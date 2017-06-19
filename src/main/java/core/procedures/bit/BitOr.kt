package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

open class BitOr : AFn<Any?, Long>(name = "bit-or", isPure = true, minArgs = 2, restArgsType = Type.BitOp::class.java) {

    override operator fun invoke(vararg args: Any?): Long {
        args[0]!!
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result or (args[i]!! as Number).toLong()
        }
        return result
    }
}
