package core.procedures.interop

import core.procedures.AFn

class Longs : AFn<Any?, LongArray>(name = "longs", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): LongArray {
        val longs = LongArray(args.size)
        for (i in 0..args.size - 1) {
            longs[i] = (args[i] as Number).toLong()
        }
        return longs
    }
}
