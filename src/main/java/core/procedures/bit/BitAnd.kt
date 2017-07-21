package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

open class BitAnd : AFn<Any?, Long>(name = "bit-and", isPure = true, minArgs = 0, restArgsType = Type.BitOp::class.java) {

    override operator fun invoke(args: Array<out Any?>) = args.fold(-1L) { r, n -> r and (n!! as Number).toLong() }
}
