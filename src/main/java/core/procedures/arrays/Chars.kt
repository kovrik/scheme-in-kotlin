package core.procedures.arrays

import core.procedures.AFn

class Chars : AFn<Any?, CharArray>(name = "chars", isPure = true, restArgsType = Char::class.javaObjectType) {

    override operator fun invoke(args: Array<out Any?>): CharArray {
        val chars = CharArray(args.size)
        for (i in 0..args.size - 1) {
            chars[i] = args[i] as Char
        }
        return chars
    }
}
