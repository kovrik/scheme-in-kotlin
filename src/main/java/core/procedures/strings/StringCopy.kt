package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableString

class StringCopy : AFn<CharSequence?, MutableString>(name = "string-copy", isPure = true, arity = Exactly(1),
                                                     mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = MutableString(arg!!.toString())
}
