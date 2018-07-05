package core.procedures.symbols

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Symbol

open class StringToSymbol : AFn<CharSequence?, Symbol>(name = "string->symbol", isPure = true, arity = Exactly(1),
                                                       mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = Symbol.intern(arg!!.toString())
}
