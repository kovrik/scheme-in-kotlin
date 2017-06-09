package core.procedures.sets

import core.exceptions.WrongTypeException
import core.procedures.AFn

class SetProc : AFn(name = "set", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Set<Any?> {
        if (arg is Collection<*>) {
            return HashSet(arg)
        }
        if (arg is CharSequence) {
            val set = HashSet<Any>(arg.length)
            for (i in 0..arg.length - 1) {
                set.add(arg[i])
            }
            return set
        }
        throw WrongTypeException(name, "List or Vector or Set or String", arg)
    }
}
