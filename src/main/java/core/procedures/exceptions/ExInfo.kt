package core.procedures.exceptions

import core.exceptions.ExInfoException
import core.procedures.AFn
import core.procedures.FnArgsBuilder

class ExInfo : AFn(FnArgsBuilder().min(2).max(3).mandatory(arrayOf(String::class.java, Map::class.java)).rest(Throwable::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "ex-info"

    override fun apply(args: Array<Any?>): ExInfoException? {
        if (args.size == 2) {
            return ExInfoException(args[0] as String, args[1] as Map<*, *>)
        }
        return ExInfoException(args[0] as String, args[1] as Map<*, *>, args[2] as Throwable)
    }
}
