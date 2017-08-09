package core.procedures.arrays

import core.procedures.AFn

class CharsLength : AFn<CharArray?, Long>(name = "chars-length", isPure = true, minArgs = 1, maxArgs = 1,
                                          mandatoryArgsTypes = arrayOf<Class<*>>(CharArray::class.java)) {

    override operator fun invoke(arg: CharArray?) = arg!!.size.toLong()
}
