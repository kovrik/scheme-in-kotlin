package core.procedures.symbols

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Symbol

open class StringToSymbol : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val isPure = true
    override val name = "string->symbol"
    override operator fun invoke(arg: Any?) = Symbol.intern(arg!!.toString())
}
