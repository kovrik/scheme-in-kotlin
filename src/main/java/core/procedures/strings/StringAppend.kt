package core.procedures.strings

import core.procedures.AFn

class StringAppend : AFn<Any?, String>(name = "string-append", isPure = true, restArgsType = CharSequence::class.java) {

    override operator fun invoke(vararg args: Any?): String {
        if (args.isEmpty()) {
            return ""
        }
        if (args.size == 1) {
            return args[0]!!.toString()
        }
        val sb = StringBuilder()
        for (arg in args) {
            sb.append(arg!!)
        }
        return sb.toString()
    }
}
