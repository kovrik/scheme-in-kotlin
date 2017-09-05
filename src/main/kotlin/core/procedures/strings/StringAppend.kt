package core.procedures.strings

import core.procedures.AFn

class StringAppend : AFn<Any?, String>(name = "string-append", isPure = true, restArgsType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.isEmpty() -> ""
        args.size == 1 -> args[0]!!.toString()
        else           -> StringBuilder().apply { args.forEach { append(it!!) } }.toString()
    }
}
