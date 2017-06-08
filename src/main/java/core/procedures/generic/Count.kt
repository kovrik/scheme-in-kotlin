package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs

open class Count : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "count"

    override operator fun invoke(arg: Any?) = when (arg) {
        is Map.Entry<*, *> -> 2
        is Map<*, *>       -> arg.size
        is Collection<*>   -> arg.size
        is CharSequence    -> arg.length
        else               -> throw WrongTypeException(name, "List or Map or Vector or Set or String", arg)
    }
}
