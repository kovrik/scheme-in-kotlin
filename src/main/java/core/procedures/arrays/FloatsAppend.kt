package core.procedures.arrays

import core.procedures.AFn

class FloatsAppend : AFn<Any?, FloatArray>(name = "floats-append", isPure = true, restArgsType = FloatArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): FloatArray {
        val floats = mutableListOf<Float>()
        for (arr in args) {
            for (b in (arr as FloatArray)) {
                floats.add(b)
            }
        }
        return floats.toFloatArray()
    }
}
