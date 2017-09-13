package core.procedures.seqs

import core.procedures.AFn
import core.scm.Type

class Repeat : AFn<Any?, Any?>(name = "repeat", isPure = true, minArgs = 1, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf<Class<*>>(Any::class.java),
                               restArgsType = Type.Real::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> generateSequence(args[0], { it })
        else -> generateSequence(args[0], { it }).take((args[1] as Number).toInt())
    }
}