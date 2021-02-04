package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Range
import core.procedures.IFn
import core.scm.Thunk
import core.scm.Type

class Repeatedly : AFn<Any?, Thunk<*>>(
    name = "repeatedly", isPure = true, arity = Range(1, 2),
    lastArgType = IFn::class.java
) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1 -> Thunk(generateSequence { invokeN<Any?, Any?>((args[0] as IFn<*, *>), emptyArray()) })
        else -> {
            Type.assertType(name, args[0], Type.Real::class.java)
            Thunk(generateSequence {
                invokeN<Any?, Any?>(args[1] as IFn<*, *>, emptyArray())
            }.take((args[0] as Number).toInt()))
        }
    }
}