package core.procedures.strings

import core.procedures.AFn

class StringAppend : AFn<Any?, String>(name = "string-append", isPure = true, restArgsType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = args.joinToString(separator = "")
}
