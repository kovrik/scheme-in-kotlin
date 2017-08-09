package core.procedures.interop

import core.procedures.AFn

class Booleans : AFn<Any?, BooleanArray>(name = "booleans", isPure = true, restArgsType = Boolean::class.javaObjectType) {

    override operator fun invoke(args: Array<out Any?>): BooleanArray {
        val booleans = BooleanArray(args.size)
        for (i in 0..args.size - 1) {
            booleans[i] = args[i] as Boolean
        }
        return booleans
    }
}
