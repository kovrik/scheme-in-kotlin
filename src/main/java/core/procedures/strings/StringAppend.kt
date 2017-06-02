package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

class StringAppend : AFn(FnArgs(rest = CharSequence::class.java)) {

    override val name = "string-append"

    override operator fun invoke(vararg args: Any?): String? {
        if (args.isEmpty()) {
            return ""
        }
        if (args.size == 1) {
            return args[0].toString()
        }
        val sb = StringBuilder()
        for (arg in args) {
            sb.append(arg)
        }
        return sb.toString()
    }
}
