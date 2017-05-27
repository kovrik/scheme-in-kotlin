package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils

class First : AFn(FnArgsBuilder().min(1).max(1).build()) {

    companion object {
        fun first(arg: Any?): Any? {
            if (arg == null) {
                return null
            }
            val iterator = Utils.toSequence(arg)
            return if (iterator.hasNext()) iterator.next() else null
        }
    }

    override val isPure = true
    override val name = "first"

    override operator fun invoke(arg: Any?): Any? {
        return first(arg)
    }
}
