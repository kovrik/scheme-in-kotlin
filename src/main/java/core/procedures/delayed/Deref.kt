package core.procedures.delayed

import core.exceptions.ArityException
import core.procedures.AFn
import core.scm.IDeref
import core.scm.Type

class Deref : AFn<Any?, Any?>(name = "deref", minArgs = 1, maxArgs = 3,
                              mandatoryArgsTypes = arrayOf(IDeref::class.java)) {

    override fun invoke(args: Array<out Any?>) = when {
        args.size == 1 -> (args[0]!! as IDeref).deref()
        args.size == 3 -> {
            Type.assertType(name, args[1], Type.Real::class.java)
            (args[0]!! as IDeref).deref((args[1] as Number).toLong(), args[2])
        }
        else -> throw ArityException("$name: arity mismatch; the expected number of arguments does not match the given number" +
                                     " (expected: 1 or 3, given: ${args.size})")
    }
}