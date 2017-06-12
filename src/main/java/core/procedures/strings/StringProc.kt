package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableString

class StringProc : AFn<Any?, MutableString>(name = "string", isPure = true, restArgsType = Char::class.javaObjectType) {

    override operator fun invoke(vararg args: Any?): MutableString {
        if (args.isEmpty()) {
            return MutableString()
        }
        val string = MutableString(args.size)
        for (c in args) {
            string.append(c!!)
        }
        return string
    }
}
