package core.procedures.arrays

import core.procedures.AFn

class ShortsFill : AFn<Any?, ShortArray>(name = "shorts-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(ShortArray::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): ShortArray {
        val b = (arg2 as Number).toShort()
        return (arg1 as ShortArray).apply { fill(b) }
    }
}
