package core.procedures.arrays

import core.procedures.AFn

class BooleansAppend : AFn<Any?, BooleanArray>(name = "booleans-append", isPure = true, restArgsType = BooleanArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): BooleanArray {
        val booleans = mutableListOf<Boolean>()
        for (arr in args) {
            for (b in (arr as BooleanArray)) {
                booleans.add(b)
            }
        }
        return booleans.toBooleanArray()
    }
}
