package core.procedures.sets

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Difference : AFn(FnArgsBuilder().min(1).mandatory(arrayOf<Class<*>>(Set::class.java)).rest(Set::class.java).build()) {

    override val isPure = true
    override val name = "difference"

    override operator fun invoke(vararg args: Any?): Set<Any?> {
        if (args.size == 1) {
            return args[0] as Set<*>
        }
        val result = HashSet(args[0] as Set<*>)
        var i = 1
        val argsLength = args.size
        while (i < argsLength) {
            result.removeAll(args[i] as Set<*>)
            i++
        }
        return result
    }
}
