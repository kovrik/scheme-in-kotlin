package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder

open class Count : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "count"

    override operator fun invoke(arg: Any?): Int? {
        when (arg) {
            is Map<*, *> -> return arg.size
            is Map.Entry<*, *> -> return 2
            is Collection<*> -> return arg.size
            is CharSequence -> return arg.length
            else -> throw WrongTypeException(name, "List or Map or Vector or Set or String", arg)
        }
    }
}
