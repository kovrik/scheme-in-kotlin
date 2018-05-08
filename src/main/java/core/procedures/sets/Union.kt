package core.procedures.sets

import core.procedures.AFn
import core.scm.MutableSet

class Union : AFn<Any?, Set<*>>(name = "union", isPure = true, restArgsType = Set::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> emptySet()
        1    -> args[0]!! as Set<*>
        else -> MutableSet(args[0]!! as Set<*>).apply {
            for (i in (1 until args.size)) {
                addAll(args[i]!! as Set<*>)
            }
        }
    }
}
