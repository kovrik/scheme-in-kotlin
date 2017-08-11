package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector
import core.utils.Utils

open class Next : AFn<Any?, Any?>(name = "next", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        null                  -> null
        !Utils.isSeqable(arg) -> throw IllegalArgumentException("don't know how to create Sequence from ${arg.javaClass}")
        is List<*>            -> if (arg.isEmpty()) null else arg.subList(1, arg.size)
        is Set<*>             -> next(arg)
        is Map<*, *>          -> next(arg.entries)
        is Map.Entry<*, *>    -> Cons.list(arg.value!!)
        is Vector             -> if (arg.size == 0) null else MutableVector(arg)
        is CharSequence       -> if (arg.length == 0) null else arg.subSequence(1, arg.length)
        is ByteArray          -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is BooleanArray       -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is CharArray          -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is ShortArray         -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is IntArray           -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is LongArray          -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is DoubleArray        -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is FloatArray         -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        is Array<*>           -> if (arg.size == 0) null else arg.copyOfRange(1, arg.size)
        else                  -> throw WrongTypeException("next", "List or Vector or Set or String or Map", arg)
    }

    private fun next(col: Collection<*>) = when {
        col.isEmpty() -> null
        else -> col.asSequence().drop(1).toSet()
    }
}
