package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableString

class StringProc : AFn(FnArgsBuilder().rest(Char::class.javaObjectType).build()) {

    override val name: String
        get() = "string"

    override operator fun invoke(vararg args: Any?): MutableString? {
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
