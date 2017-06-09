package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

class BitClear : AFn(name = "bit-clear", isPure = true, minArgs = 2, maxArgs = 2,
                     mandatoryArgsTypes = arrayOf(Type.BitOp::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Long {
        val number = (arg1 as Number).toLong()
        return number and (1L shl (arg2 as Number).toInt()).inv()
    }
}
