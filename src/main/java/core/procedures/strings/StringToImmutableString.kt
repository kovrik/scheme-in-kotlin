package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly

class StringToImmutableString : AFn<Any?, String>(name = "string->immutable-string", isPure = true, arity = Exactly(1),
                                                  mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = arg!!.toString()
}