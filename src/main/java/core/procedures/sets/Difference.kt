package core.procedures.sets

import core.procedures.AFn

class Difference : AFn<Any?, Set<*>>(name = "difference", isPure = true, minArgs = 1,
                       mandatoryArgsTypes = arrayOf<Class<*>>(Set::class.java), restArgsType = Set::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size == 1 -> args[0]!! as Set<*>
        else -> HashSet(args[0]!! as Set<*>).apply {
            for (i in (1..args.size - 1)) {
                removeAll(args[i]!! as Set<*>)
            }
        }
    }
}
