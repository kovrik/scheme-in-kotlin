package core.procedures.sets

import core.exceptions.WrongTypeException
import core.procedures.AFn

class SetProc : AFn<Any?, Set<*>>(name = "set", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Collection<*> -> HashSet(arg)
        is CharSequence  -> HashSet<Any>(arg.length).apply { addAll(arg.asSequence()) }
        else -> throw WrongTypeException(name, "List or Vector or Set or String", arg)
    }
}
