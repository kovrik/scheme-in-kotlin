package core.procedures.arrays

import core.procedures.AFn

class BooleansFill : AFn<Any?, BooleanArray>(name = "booleans-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(BooleanArray::class.java, Boolean::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): BooleanArray {
        val b = arg2 as Boolean
        return (arg1 as BooleanArray).apply { fill(b) }
    }
}
