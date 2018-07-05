package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Includes : AFn<CharSequence?, Boolean>(name = "includes?", isPure = true, arity = Exactly(2),
                                             mandatoryArgsTypes = arrayOf(CharSequence::class.java,
                                                                          CharSequence::class.java)) {

    override operator fun invoke(arg1: CharSequence?, arg2: CharSequence?) = arg1!!.contains(arg2!!)
}
