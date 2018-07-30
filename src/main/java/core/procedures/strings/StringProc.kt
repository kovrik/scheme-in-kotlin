package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableString

class StringProc : AFn<Any?, MutableString>(name = "string", isPure = true, restArgsType = Char::class.javaObjectType) {

    override operator fun invoke(args: Array<out Any?>) = args.joinTo(MutableString(), separator = "")
}
