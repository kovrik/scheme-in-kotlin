package core.procedures.sets

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs

class SetProc : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "set"

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
