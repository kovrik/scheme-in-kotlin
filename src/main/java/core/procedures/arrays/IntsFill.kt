package core.procedures.arrays

import core.procedures.AFn

class IntsFill : AFn<Any?, IntArray>(name = "ints-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(IntArray::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): IntArray {
        val b = (arg2 as Number).toInt()
        return (arg1 as IntArray).apply { fill(b) }
    }
}
