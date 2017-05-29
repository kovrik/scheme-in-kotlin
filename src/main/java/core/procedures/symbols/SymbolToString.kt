package core.procedures.symbols

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Symbol

class SymbolToString : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Symbol::class.java))) {

    override val isPure = true
    override val name = "symbol->string"

    override operator fun invoke(arg: Any?): String? {
        return arg!!.toString()
    }
}
