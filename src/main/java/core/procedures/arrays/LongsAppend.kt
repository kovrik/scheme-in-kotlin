package core.procedures.arrays

import core.procedures.AFn

class LongsAppend : AFn<Any?, LongArray>(name = "longs-append", isPure = true, restArgsType = LongArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): LongArray {
        val longs = mutableListOf<Long>()
        for (arr in args) {
            for (b in (arr as LongArray)) {
                longs.add(b)
            }
        }
        return longs.toLongArray()
    }
}
