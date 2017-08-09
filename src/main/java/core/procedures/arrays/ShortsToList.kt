package core.procedures.arrays

import core.procedures.AFn

class ShortsToList : AFn<ShortArray?, List<Short>?>(name = "shorts->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                    mandatoryArgsTypes = arrayOf<Class<*>>(ShortArray::class.java)) {

    override operator fun invoke(arg: ShortArray?) = arg?.toList()
}
