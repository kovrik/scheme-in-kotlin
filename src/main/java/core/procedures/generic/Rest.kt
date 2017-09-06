package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.Vector
import core.utils.Utils

open class Rest : AFn<Any?, Any?>(name = "rest", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        null                  -> Cons.EMPTY
        !Utils.isSeqable(arg) -> throw IllegalArgumentException("don't know how to create Sequence from ${arg.javaClass}")
        is List<*>            -> if (arg.size   < 2) Cons.EMPTY       else arg.subList(1, arg.size)
        is Set<*>             -> rest(arg)
        is Map<*, *>          -> rest(arg.entries)
        is Map.Entry<*, *>    -> Cons.list(arg.value!!)
        is Vector             -> if (arg.size   < 2) Vector()         else Vector(arg, 1, arg.size)
        is CharSequence       -> if (arg.length < 2) ""               else arg.subSequence(1, arg.length)
        is ByteArray          -> if (arg.size   < 2) byteArrayOf()    else arg.copyOfRange(1, arg.size)
        is BooleanArray       -> if (arg.size   < 2) booleanArrayOf() else arg.copyOfRange(1, arg.size)
        is CharArray          -> if (arg.size   < 2) charArrayOf()    else arg.copyOfRange(1, arg.size)
        is ShortArray         -> if (arg.size   < 2) shortArrayOf()   else arg.copyOfRange(1, arg.size)
        is IntArray           -> if (arg.size   < 2) intArrayOf()     else arg.copyOfRange(1, arg.size)
        is LongArray          -> if (arg.size   < 2) longArrayOf()    else arg.copyOfRange(1, arg.size)
        is DoubleArray        -> if (arg.size   < 2) doubleArrayOf()  else arg.copyOfRange(1, arg.size)
        is FloatArray         -> if (arg.size   < 2) floatArrayOf()   else arg.copyOfRange(1, arg.size)
        is Array<*>           -> if (arg.size   < 2) arrayOfNulls(0)  else arg.copyOfRange(1, arg.size)
        else                  -> throw WrongTypeException("rest", "List or Vector or Set or String or Map", arg)
    }

    private fun rest(col: Set<*>): Set<Any?> = when {
        col.size < 2  -> emptySet()
        else          -> col.asSequence().drop(1).toSet()
    }
}
