package core.procedures.sets

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder

class SetProc : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure = true
    override val name = "set"

    override operator fun invoke(arg: Any?): Set<Any?> {
        if (arg is Collection<*>) {
            return HashSet(arg)
        }
        if (arg is CharSequence) {
            val set = HashSet<Any>(arg.length)
            val cs = arg
            for (i in 0..cs.length - 1) {
                set.add(cs[i])
            }
            return set
        }
        throw WrongTypeException(name, "List or Vector or Set or String", arg)
    }
}
