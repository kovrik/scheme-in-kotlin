package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Void
import java.util.*

class Merge : AFn(FnArgsBuilder().min(0).rest(Map::class.java).build()) {

    override val isPure = true
    override val name = "merge"

    override operator fun invoke(vararg args: Any?): Any? {
        if (args.isEmpty()) {
            return Void
        }
        val result = HashMap<Any?, Any?>()
        for (m in args) {
            result.putAll(m as Map<*, *>)
        }
        return result
    }
}
