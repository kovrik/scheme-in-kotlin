package core.procedures.sets

import core.procedures.AFn

class Intersection : AFn<Any?, Set<*>>(name = "intersection", isPure = true, minArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(Set::class.java), restArgsType = Set::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size == 1 -> args[0]!! as Set<*>
        else -> HashSet(args[0]!! as Set<*>).apply {
            for (i in (1..args.size - 1)) {
                retainAll(args[i]!! as Set<*>)
            }
        }
    }
}
