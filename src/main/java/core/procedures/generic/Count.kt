package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn

open class Count : AFn<Any?, Int>(name = "count", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Map.Entry<*, *> -> 2
        is Map<*, *>       -> arg.size
        is Collection<*>   -> arg.size
        is CharSequence    -> arg.length
        else               -> throw WrongTypeException(name, "List or Map or Vector or Set or String", arg)
    }
}
