package core.procedures.symbols

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Symbol

class SymbolToString : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Symbol::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "symbol->string"

    override operator fun invoke(arg: Any?): String? {
        return arg!!.toString()
    }
}
