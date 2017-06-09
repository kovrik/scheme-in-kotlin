package core.procedures.strings

import core.procedures.AFn

class StringToImmutableString : AFn(name = "string->immutable-string", isPure = true, minArgs = 1, maxArgs = 1,
                                    mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is String -> arg
        else      -> arg!!.toString()
    }
}