package core.procedures.arrays

import core.procedures.AFn

class LongsFill : AFn<Any?, LongArray>(name = "longs-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(LongArray::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): LongArray {
        val b = (arg2 as Number).toLong()
        return (arg1 as LongArray).apply { fill(b) }
    }
}
