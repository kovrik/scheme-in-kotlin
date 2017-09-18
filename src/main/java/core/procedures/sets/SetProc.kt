package core.procedures.sets

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.MutableHashSet

class SetProc : AFn<Any?, Set<*>>(name = "set", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Collection<*> -> MutableHashSet(arg)
        is CharSequence  -> MutableHashSet<Any?>(arg.length).apply { addAll(arg.asSequence()) }
        else -> throw WrongTypeException(name, "List or Vector or Set or String", arg)
    }
}
