package core.procedures.arrays

import core.procedures.AFn

class Ints : AFn<Any?, IntArray>(name = "ints", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): IntArray {
        val ints = IntArray(args.size)
        for (i in 0..args.size - 1) {
            ints[i] = (args[i] as Number).toInt()
        }
        return ints
    }
}
