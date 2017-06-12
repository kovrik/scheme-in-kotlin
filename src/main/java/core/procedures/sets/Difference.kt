package core.procedures.sets

import core.procedures.AFn

class Difference : AFn<Set<*>?, Set<*>>(name = "difference", isPure = true, minArgs = 1,
                       mandatoryArgsTypes = arrayOf<Class<*>>(Set::class.java), restArgsType = Set::class.java) {

    override operator fun invoke(vararg args: Set<*>?): Set<*> {
        if (args.size == 1) {
            return args[0]!!
        }
        val result = HashSet(args[0]!!)
        var i = 1
        val argsLength = args.size
        while (i < argsLength) {
            result.removeAll(args[i]!!)
            i++
        }
        return result
    }
}
