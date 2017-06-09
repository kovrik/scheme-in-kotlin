package core.procedures.symbols

import core.procedures.AFn
import core.scm.Symbol

class SymbolToString : AFn(name = "symbol->string", isPure = true, minArgs = 1, maxArgs = 1,
                           mandatoryArgsTypes = arrayOf<Class<*>>(Symbol::class.java)) {

    override operator fun invoke(arg: Any?) = arg!!.toString()
}
