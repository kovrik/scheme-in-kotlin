package core.procedures.symbols

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Symbol

open class StringToSymbol : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "string->symbol"

    override operator fun invoke(arg: Any?): Symbol? {
        return Symbol.intern(arg!!.toString())
    }
}
