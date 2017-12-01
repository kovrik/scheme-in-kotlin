package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

open class BitNot : AFn<Number?, Long>(name = "bit-not", isPure = true, minArgs = 1, maxArgs = 1,
                                       mandatoryArgsTypes = arrayOf(Type.BitOp::class.java)) {

    override operator fun invoke(arg: Number?) = arg!!.toLong().inv()
}
