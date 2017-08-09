package core.procedures.arrays

import core.procedures.AFn

class CharsFill : AFn<Any?, CharArray>(name = "chars-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(CharArray::class.java, Char::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): CharArray {
        val b = arg2 as Char
        return (arg1 as CharArray).apply { fill(b) }
    }
}
