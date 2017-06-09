package core.procedures.bit

import core.procedures.AFn
import core.scm.Type

open class BitNot : AFn(name = "bit-not", isPure = true, minArgs = 1, maxArgs = 1,
                        mandatoryArgsTypes = arrayOf<Class<*>>(Type.BitOp::class.java)) {

    override operator fun invoke(arg: Any?) = (arg as Number).toLong().inv()
}
