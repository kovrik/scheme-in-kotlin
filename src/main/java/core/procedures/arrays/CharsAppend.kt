package core.procedures.arrays

import core.procedures.AFn

class CharsAppend : AFn<Any?, CharArray>(name = "chars-append", isPure = true, restArgsType = CharArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): CharArray {
        val chars = mutableListOf<Char>()
        for (arr in args) {
            for (b in (arr as CharArray)) {
                chars.add(b)
            }
        }
        return chars.toCharArray()
    }
}
