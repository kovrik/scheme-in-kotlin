package core.procedures.sets

import core.procedures.AFn

class Union : AFn<Set<*>?, Set<*>>(name = "union", isPure = true, restArgsType = Set::class.java) {

    override operator fun invoke(vararg args: Set<*>?): Set<*> {
        if (args.isEmpty()) {
            return emptySet<Any?>()
        }
        if (args.size == 1) {
            return args[0]!!
        }
        val result = HashSet(args[0]!!)
        var i = 1
        val argsLength = args.size
        while (i < argsLength) {
            result.addAll(args[i]!!)
            i++
        }
        return result
    }
}
