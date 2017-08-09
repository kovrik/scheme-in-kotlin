package core.procedures.arrays

import core.procedures.AFn

class CharsToList : AFn<CharArray?, List<Char>?>(name = "chars->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                 mandatoryArgsTypes = arrayOf<Class<*>>(CharArray::class.java)) {

    override operator fun invoke(arg: CharArray?) = arg?.toList()
}
