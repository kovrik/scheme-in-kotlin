package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

class BitFlip : AFn(name = "bit-flip", isPure = true, minArgs = 2, maxArgs = 2,
                    mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Long {
        val number = (arg1 as Number).toLong()
        return number xor (1L shl (arg2 as Number).toInt())
    }
}
