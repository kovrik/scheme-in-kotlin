package core.procedures.bit

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

open class BitTest : AFn<Number?, Boolean>(name = "bit-test", isPure = true, arity = Exactly(2),
                                           mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?) = (arg1!!.toInt() shr arg2!!.toInt() and 1) == 1
}
