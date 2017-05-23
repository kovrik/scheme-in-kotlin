package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableString

class StringProc : AFn(FnArgsBuilder().rest(Char::class.javaObjectType).build()) {

    override val name: String
        get() = "string"

    override fun apply(args: Array<Any?>): MutableString? {
        if (args.isEmpty()) {
            return MutableString()
        }
        val string = MutableString(args.size)
        for (c in args) {
            string.append(c)
        }
        return string
    }
}
