package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

class StringRef : AFn<Any?, Char>(name = "string-ref", isPure = true, arity = Exactly(2),
                                  mandatoryArgsTypes = arrayOf(CharSequence::class.java,
                                                               Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg2 as Number).toInt().let {
        arg1!!.toString().getOrElse(it) { throw IndexOutOfBoundsException("$name: value out of range: $it") }
    }
}
