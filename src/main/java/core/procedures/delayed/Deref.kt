package core.procedures.delayed

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.Arity.OneOf
import core.scm.IDeref
import core.scm.Type

class Deref : AFn<Any?, Any?>(name = "deref", arity = OneOf(listOf(Exactly(1), Exactly(3))),
                              mandatoryArgsTypes = arrayOf(IDeref::class.java)) {

    override fun invoke(args: Array<out Any?>) = when (args.size) {
        1 -> (args[0]!! as IDeref).deref()
        3 -> {
            Type.assertType(name, args[1], Type.Real::class.java)
            (args[0]!! as IDeref).deref((args[1] as Number).toLong(), args[2])
        }
        else -> Unit
    }
}