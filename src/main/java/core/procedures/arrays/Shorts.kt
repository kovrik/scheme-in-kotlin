package core.procedures.arrays

import core.procedures.AFn

class Shorts : AFn<Any?, ShortArray>(name = "shorts", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): ShortArray {
        val shorts = ShortArray(args.size)
        for (i in 0..args.size - 1) {
            shorts[i] = (args[i] as Number).toShort()
        }
        return shorts
    }
}
