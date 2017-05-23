package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils

class Second : AFn(FnArgsBuilder().min(1).max(1).build()) {

    companion object {
        fun second(arg: Any?): Any? {
            if (arg == null) {
                return null
            }
            val iterator = Utils.toSequence(arg)
            if (iterator.hasNext()) {
                iterator.next()
                return if (iterator.hasNext()) iterator.next() else null
            }
            return null
        }
    }

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "second"

    override fun apply1(arg: Any?): Any? {
        return second(arg)
    }
}
