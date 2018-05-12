package core.procedures.strings

import core.procedures.AFn

class StringToList : AFn<CharSequence?, List<Char?>>(name = "string->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                     mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg!!.toList()
}