package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Void

import java.util.HashMap

class Merge : AFn(FnArgsBuilder().min(0).rest(Map::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "merge"

    override operator fun invoke(vararg args: Any?): Any? {
        if (args.isEmpty()) {
            return Void.VOID
        }
        val result = HashMap<Any?, Any?>()
        for (m in args) {
            result.putAll(m as Map<*, *>)
        }
        return result
    }
}
