package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.Utils

class First : AFn(FnArgs(min = 1, max = 1)) {

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
