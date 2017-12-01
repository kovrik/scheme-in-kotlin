package core.procedures.strings

import core.procedures.AFn

class StringToImmutableString : AFn<Any?, String>(name = "string->immutable-string", isPure = true, minArgs = 1, maxArgs = 1,
                                                  mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = arg!!.toString()
}