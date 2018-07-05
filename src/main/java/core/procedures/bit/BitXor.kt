package core.procedures.bit

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.Type

open class BitXor : AFn<Any?, Long?>(name = "bit-xor", isPure = true, arity = AtLeast(0), restArgsType = Type.BitOp::class.java) {

    override operator fun invoke(args: Array<out Any?>) = args.fold(0L) { r, n -> r xor (n!! as Number).toLong() }
}
