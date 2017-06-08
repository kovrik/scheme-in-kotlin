package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

class Conj : AFn(FnArgs(min = 1)) {

    override val name = "conj"

    override operator fun invoke(vararg args: Any?): Any? {
        val first = args[0]
        when {
            args.size == 1 -> return first
            first is List<*> -> {
                val list = Cons.list<Any?>()
                list.addAll(first)
                list.addAll(args.copyOfRange(1, args.size))
                return list
            }
            first is Set<*> -> {
                val set = HashSet<Any?>()
                set.addAll(first as Collection<*>)
                set.addAll(args.copyOfRange(1, args.size))
                return set
            }
            first is Vector -> {
                val vector = MutableVector(first.size + args.size - 1, null)
                for (i in 0..first.size - 1) {
                    vector[i] = first[i]
                }
                System.arraycopy(args, 1, vector.getArray(), first.size, args.size - 1)
                return vector
            }
            // TODO Map?
            else -> throw WrongTypeException(name, "List or Vector or Set", first)
        }
    }
}
