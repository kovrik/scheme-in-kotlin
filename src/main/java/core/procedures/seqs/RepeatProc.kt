package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.Repeat
import core.scm.Type

class RepeatProc : AFn<Any?, Any?>(name = "repeat", arity = Range(1, 2)) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> Repeat(args[0])
        else -> {
            Type.assertType(name, args[0], Type.Real::class.java)
            Repeat(args[1]).take((args[0] as Number).toInt())
        }
    }
}