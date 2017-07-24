package core.procedures.sets

import core.procedures.AFn

class Union : AFn<Any?, Set<*>>(name = "union", isPure = true, restArgsType = Set::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.isEmpty() -> emptySet<Any?>()
        args.size == 1 -> args[0]!! as Set<*>
        else -> HashSet(args[0]!! as Set<*>).apply {
            for (i in (1..args.size - 1)) {
                addAll(args[i]!! as Set<*>)
            }
        }
    }
}
