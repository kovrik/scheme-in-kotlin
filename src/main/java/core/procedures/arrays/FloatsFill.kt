package core.procedures.arrays

import core.procedures.AFn

class FloatsFill : AFn<Any?, FloatArray>(name = "floats-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(FloatArray::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): FloatArray {
        val b = (arg2 as Number).toFloat()
        return (arg1 as FloatArray).apply { fill(b) }
    }
}
