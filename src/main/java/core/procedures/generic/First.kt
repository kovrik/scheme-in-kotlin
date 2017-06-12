package core.procedures.generic

import core.procedures.AFn
import core.utils.Utils

class First : AFn<Any?, Any?>(name = "first", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = first(arg)

    companion object {
        fun first(arg: Any?): Any? {
            if (arg == null) return null
            val iterator = Utils.toSequence(arg)
            return if (iterator.hasNext()) iterator.next() else null
        }
    }
}
