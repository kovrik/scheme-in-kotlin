package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Trim : AFn<CharSequence?, String>(name = "trim", isPure = true, arity = Exactly(1),
                                        mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg!!.toString().trim()
}
