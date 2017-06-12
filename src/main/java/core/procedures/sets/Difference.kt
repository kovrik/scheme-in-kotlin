package core.procedures.sets

import core.procedures.AFn

class Difference : AFn<Any?, Set<*>>(name = "difference", isPure = true, minArgs = 1,
                       mandatoryArgsTypes = arrayOf<Class<*>>(Set::class.java), restArgsType = Set::class.java) {

    override operator fun invoke(vararg args: Any?): Set<*> {
        if (args.size == 1) {
            return args[0]!! as Set<*>
        }
        val result = HashSet(args[0]!! as Set<*>)
        var i = 1
        val argsLength = args.size
        while (i < argsLength) {
            result.removeAll(args[i]!! as Set<*>)
            i++
        }
        return result
    }
}
