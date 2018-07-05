package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly

class StringToList : AFn<CharSequence?, List<Char?>>(name = "string->list", isPure = true, arity = Exactly(1),
                                                     mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg!!.toList()
}