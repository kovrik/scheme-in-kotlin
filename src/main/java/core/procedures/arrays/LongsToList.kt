package core.procedures.arrays

import core.procedures.AFn

class LongsToList : AFn<LongArray?, List<Long>?>(name = "longs->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                 mandatoryArgsTypes = arrayOf<Class<*>>(LongArray::class.java)) {

    override operator fun invoke(arg: LongArray?) = arg?.toList()
}
