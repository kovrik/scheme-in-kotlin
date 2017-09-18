package core.procedures.sets

import core.procedures.AFn
import core.scm.MutableHashSet

class Difference : AFn<Any?, Set<*>>(name = "difference", isPure = true, minArgs = 1,
                                     mandatoryArgsTypes = arrayOf<Class<*>>(Set::class.java),
                                     restArgsType = Set::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size == 1 -> args[0]!! as Set<*>
        else -> MutableHashSet(args[0]!! as Set<*>).apply {
            for (i in (1 until args.size)) {
                removeAll(args[i]!! as Set<*>)
            }
        }
    }
}
