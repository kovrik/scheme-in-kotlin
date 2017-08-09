package core.procedures.arrays

import core.procedures.AFn

class IntsToList : AFn<IntArray?, List<Int>?>(name = "ints->list", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(IntArray::class.java)) {

    override operator fun invoke(arg: IntArray?) = arg?.toList()
}
