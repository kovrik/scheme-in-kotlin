package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

open class BitTest : AFn(name = "bit-test", isPure = true, minArgs = 2, maxArgs = 2,
                         mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        val n = (arg2 as Number).toInt()
        return ((arg1 as Number).toInt() shr n and 1) == 1
    }
}
