package core.procedures.arrays

import core.procedures.AFn

class ShortsAppend : AFn<Any?, ShortArray>(name = "shorts-append", isPure = true, restArgsType = ShortArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): ShortArray {
        val shorts = mutableListOf<Short>()
        for (arr in args) {
            for (b in (arr as ShortArray)) {
                shorts.add(b)
            }
        }
        return shorts.toShortArray()
    }
}
