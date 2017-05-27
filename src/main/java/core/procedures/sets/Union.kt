package core.procedures.sets

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Union : AFn(FnArgsBuilder().min(0).rest(Set::class.java).build()) {

    override val isPure = true
    override val name = "union"

    override operator fun invoke(vararg args: Any?): Set<Any?> {
        if (args.isEmpty()) {
            return HashSet()
        }
        if (args.size == 1) {
            return args[0] as Set<*>
        }
        val result = HashSet(args[0] as Set<*>)
        var i = 1
        val argsLength = args.size
        while (i < argsLength) {
            result.addAll(args[i] as Set<*>)
            i++
        }
        return result
    }
}
