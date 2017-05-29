package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableString

class StringProc : AFn(FnArgs(rest = Char::class.javaObjectType)) {

    override val name = "string"

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
