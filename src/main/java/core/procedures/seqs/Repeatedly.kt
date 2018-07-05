package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Range
import core.procedures.IFn
import core.scm.ThunkSeq
import core.scm.Type

class Repeatedly : AFn<Any?, Sequence<*>>(name = "repeatedly", isPure = true, arity = Range(1, 2),
                                          lastArgType = IFn::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1 -> ThunkSeq(generateSequence<Any>(Unit, {
            AFn.invokeN<Any?, Any?>((args[0] as IFn<*, *>), emptyArray())
        }).drop(1))
        else -> {
            Type.assertType(name, args[0], Type.Real::class.java)
            ThunkSeq(generateSequence<Any>(Unit, {
                AFn.invokeN<Any?, Any?>(args[1] as IFn<*, *>, emptyArray())
            })).drop(1).take((args[0] as Number).toInt())
        }
    }
}