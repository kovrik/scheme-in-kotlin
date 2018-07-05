package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly

class StringLength : AFn<CharSequence?, Long>(name = "string-length", isPure = true, arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg!!.length.toLong()
}
