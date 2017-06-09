package core.procedures.symbols

import core.procedures.AFn
import core.scm.Symbol

open class StringToSymbol : AFn(name = "string->symbol", isPure = true, minArgs = 1, maxArgs = 1,
                                mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = Symbol.intern(arg!!.toString())
}
