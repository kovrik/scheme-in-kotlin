package core.procedures.arrays

import core.procedures.AFn

class ShortsLength : AFn<ShortArray?, Short>(name = "shorts-length", isPure = true, minArgs = 1, maxArgs = 1,
                                             mandatoryArgsTypes = arrayOf<Class<*>>(ShortArray::class.java)) {

    override operator fun invoke(arg: ShortArray?) = arg!!.size.toShort()
}
