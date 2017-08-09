package core.procedures.interop

import core.procedures.AFn

class Doubles : AFn<Any?, DoubleArray>(name = "doubles", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): DoubleArray {
        val doubles = DoubleArray(args.size)
        for (i in 0..args.size - 1) {
            doubles[i] = (args[i] as Number).toDouble()
        }
        return doubles
    }
}
