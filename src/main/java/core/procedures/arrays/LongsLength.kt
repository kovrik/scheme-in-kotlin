package core.procedures.arrays

import core.procedures.AFn

class LongsLength : AFn<LongArray?, Long>(name = "longs-length", isPure = true, minArgs = 1, maxArgs = 1,
                                          mandatoryArgsTypes = arrayOf<Class<*>>(LongArray::class.java)) {

    override operator fun invoke(arg: LongArray?) = arg!!.size.toLong()
}
