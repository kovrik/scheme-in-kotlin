package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

class Conj : AFn<Any?, Any?>(name = "conj", minArgs = 1) {

    override operator fun invoke(args: Array<out Any?>): Any? {
        val first = args[0]
        return when {
            args.size == 1 -> first
            first is List<*> -> Cons.list<Any?>().apply {
                addAll(first)
                addAll(args.copyOfRange(1, args.size))
            }
            first is Set<*> -> HashSet<Any?>().apply {
                addAll(first as Collection<*>)
                addAll(args.copyOfRange(1, args.size))
            }
            first is Vector -> MutableVector(first.size + args.size - 1, null).apply {
                for (i in 0..first.size - 1) {
                    this[i] = first[i]
                }
                System.arraycopy(args, 1, this.getArray(), first.size, args.size - 1)
            }
            first is ByteArray -> ByteArray(first.size + args.size - 1).apply {
                for (i in 0..first.size - 1) {
                    this[i] = first[i]
                }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            // TODO Map?
            else -> throw WrongTypeException(name, "List or Vector or Set or ByteArray", first)
        }
    }
}
