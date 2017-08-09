package core.procedures.arrays

import core.procedures.AFn

class DoublesFill : AFn<Any?, DoubleArray>(name = "doubles-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(DoubleArray::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): DoubleArray {
        val b = (arg2 as Number).toDouble()
        return (arg1 as DoubleArray).apply { fill(b) }
    }
}
