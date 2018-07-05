package core.procedures.meta

import core.procedures.AFn
import core.procedures.Arity
import core.procedures.Arity.Exactly
import core.procedures.IFn

class ArityProc : AFn<IFn<*, *>, Arity>(name = "arity", arity = Exactly(1), mandatoryArgsTypes = arrayOf(IFn::class.java)) {

    override operator fun invoke(arg: IFn<*, *>) = arg.arity()
}
