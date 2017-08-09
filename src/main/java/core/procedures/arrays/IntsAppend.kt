package core.procedures.arrays

import core.procedures.AFn

class IntsAppend : AFn<Any?, IntArray>(name = "ints-append", isPure = true, restArgsType = IntArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): IntArray {
        val ints = mutableListOf<Int>()
        for (arr in args) {
            for (b in (arr as IntArray)) {
                ints.add(b)
            }
        }
        return ints.toIntArray()
    }
}
