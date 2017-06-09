package core.procedures.generic

import core.procedures.AFn
import core.utils.Utils

class Second : AFn(name = "second", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = second(arg)

    companion object {
        fun second(arg: Any?): Any? {
            if (arg != null) {
                val iterator = Utils.toSequence(arg)
                if (iterator.hasNext()) {
                    iterator.next()
                    return if (iterator.hasNext()) iterator.next() else null
                }
            }
            return null
        }
    }
}
