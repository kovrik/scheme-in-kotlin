package core.procedures.symbols

import core.procedures.AFn
import core.scm.Symbol

class SymbolToString : AFn<Symbol?, String>(name = "symbol->string", isPure = true, minArgs = 1, maxArgs = 1,
                                            mandatoryArgsTypes = arrayOf(Symbol::class.java)) {

    override operator fun invoke(arg: Symbol?) = arg!!.toString()
}
