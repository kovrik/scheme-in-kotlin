package core.procedures.sets

import core.procedures.AFn

class Union : AFn<Any?, Set<*>>(name = "union", isPure = true, restArgsType = Set::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.isEmpty() -> emptySet<Any?>()
        args.size == 1 -> args[0]!! as Set<*>
        else -> {
            val result = HashSet(args[0]!! as Set<*>)
            var i = 1
            val argsLength = args.size
            while (i < argsLength) {
                result.addAll(args[i]!! as Set<*>)
                i++
            }
            result
        }
    }
}
