package core.procedures.arrays

import core.procedures.AFn

class Floats : AFn<Any?, FloatArray>(name = "floats", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): FloatArray {
        val floats = FloatArray(args.size)
        for (i in 0..args.size - 1) {
            floats[i] = (args[i] as Number).toFloat()
        }
        return floats
    }
}
