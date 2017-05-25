package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class StringAppend : AFn(FnArgsBuilder().rest(CharSequence::class.java).build()) {

    override val name: String
        get() = "string-append"

    override operator fun invoke(vararg args: Any?): String? {
        if (args.isEmpty()) {
            return ""
        }
        if (args.size == 1) {
            val o = args[0]
            return o.toString()
        }
        val sb = StringBuilder()
        for (arg in args) {
            sb.append(arg)
        }
        return sb.toString()
    }
}
