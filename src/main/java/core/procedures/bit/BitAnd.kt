package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

open class BitAnd : AFn<Any?, Long>(name = "bit-and", isPure = true, minArgs = 2, restArgsType = Type.BitOp::class.java) {

    override operator fun invoke(args: Array<out Any?>): Long {
        args[0]!!
        var result = (args[0] as Number).toLong()
        for (i in 1..args.size - 1) {
            result = result and (args[i]!! as Number).toLong()
        }
        return result
    }
}
