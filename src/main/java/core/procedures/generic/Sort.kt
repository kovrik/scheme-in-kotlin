package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Vector
import java.util.*

class Sort : AFn<Any?, Any?>(name = "sort", isPure = true, minArgs = 1, maxArgs = 2) {

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.size == 1 || args[0] == null) {
            return (args[0] ?: args[1]).let {
                try {
                    when (it) {
                        is List<*>     -> (it as List<Comparable<Any?>>).toMutableList().apply { sort() }
                        is Vector      -> MutableVector(it).apply { array.sort() }
                        is CharArray   -> it.copyOf().apply { sort() }
                        is ByteArray   -> it.copyOf().apply { sort() }
                        is ShortArray  -> it.copyOf().apply { sort() }
                        is IntArray    -> it.copyOf().apply { sort() }
                        is LongArray   -> it.copyOf().apply { sort() }
                        is FloatArray  -> it.copyOf().apply { sort() }
                        is DoubleArray -> it.copyOf().apply { sort() }
                        is Array<*>    -> it.copyOf().apply { sort() }
                        is Map<*, *>   -> TreeMap(it as Map<*, *>?)
                        else           -> throw WrongTypeException(name, "Collection of comparable elements", it)
                    }
                } catch (e: ClassCastException) {
                    throw WrongTypeException(name, "Collection of comparable elements", it)
                }
            }
        }
        val comparator = args[0] as Comparator<Any?>
        return args[1].let {
            try {
                when (it) {
                    is List<*>     -> (it as List<Comparable<Any?>>).toMutableList().apply { sortWith(comparator) }
                    is Vector      -> MutableVector(it).apply { array.sortWith(comparator) }
                    is CharArray   -> it.copyOf().sortedWith(comparator).toTypedArray()
                    is ByteArray   -> it.copyOf().sortedWith(comparator).toTypedArray()
                    is ShortArray  -> it.copyOf().sortedWith(comparator).toTypedArray()
                    is IntArray    -> it.copyOf().sortedWith(comparator).toTypedArray()
                    is LongArray   -> it.copyOf().sortedWith(comparator).toTypedArray()
                    is FloatArray  -> it.copyOf().sortedWith(comparator).toTypedArray()
                    is DoubleArray -> it.copyOf().sortedWith(comparator).toTypedArray()
                    is Array<*>    -> it.copyOf().apply { sortWith(comparator) }
                    is Map<*, *>   -> TreeMap<Any?, Any?>(comparator).apply { putAll(it) }
                    else           -> throw WrongTypeException(name, "Collection of comparable elements", it)
                }
            } catch (e: ClassCastException) {
                throw WrongTypeException(name, "Collection of comparable elements", it)
            }
        }
    }
}
