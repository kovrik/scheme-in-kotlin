package core.procedures.symbols

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Symbol

class SymbolToString : AFn<Symbol?, String>(name = "symbol->string", isPure = true, arity = Exactly(1),
                                            mandatoryArgsTypes = arrayOf(Symbol::class.java)) {

    override operator fun invoke(arg: Symbol?) = arg!!.toString()
}
