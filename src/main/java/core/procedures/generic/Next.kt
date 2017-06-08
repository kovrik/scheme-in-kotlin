package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.Vector
import core.utils.Utils

open class Next : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "next"

    override operator fun invoke(arg: Any?) = when (arg) {
        null                  -> null
        !Utils.isSeqable(arg) -> throw IllegalArgumentException("don't know how to create Sequence from " + arg.javaClass)
        is List<*>            -> if (arg.isEmpty()) null else arg.subList(1, arg.size)
        is Set<*>             -> next(arg)
        is Map<*, *>          -> next(arg.entries)
        is Map.Entry<*, *>    -> Cons.list(arg.value!!)
        is Vector             -> if (arg.size == 0) null else Vector(arg.getArray().copyOfRange(1, arg.size))
        is CharSequence       -> if (arg.length == 0) null else arg.subSequence(1, arg.length)
        else                  -> throw WrongTypeException("next", "List or Vector or Set or String or Map", arg)
    }

    private fun next(set: Set<*>): Any? {
        if (set.isEmpty()) {
            return null
        }
        val iter = set.iterator()
        iter.next()
        val next = HashSet<Any?>()
        while (iter.hasNext()) {
            next.add(iter.next())
        }
        return next
    }
}
