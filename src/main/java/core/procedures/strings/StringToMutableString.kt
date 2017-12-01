package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableString

class StringToMutableString : AFn<CharSequence?, CharSequence>(name = "string->mutable-string", isPure = true,
                                                               minArgs = 1, maxArgs = 1,
                                                               mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = when (arg) {
        is MutableString, is StringBuilder -> arg
        else -> MutableString(arg!!.toString())
    }
}