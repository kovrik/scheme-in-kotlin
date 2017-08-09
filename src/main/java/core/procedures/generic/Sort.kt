package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Vector
import java.util.*

class Sort : AFn<Any?, Any?>(name = "sort", isPure = true, minArgs = 1, maxArgs = 2) {

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.size == 1 || args[0] == null) {
            return (args[0] ?: args[1]).let {
                try {
                    when (it) {
                        is List<*>   -> (it as List<Comparable<Any?>>).toMutableList().apply { sort() }
                        is Vector    -> Vector(it.getArray().copyOf().apply { sort() })
                        is ByteArray -> it.copyOf().apply { sort() }
                        is Map<*, *> -> TreeMap(it as Map<*, *>?)
                        else         -> throw WrongTypeException(name, "Collection of comparable elements", it)
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
                    is List<*>   -> (it as List<Comparable<Any?>>).toMutableList().apply { sortWith(comparator) }
                    is Vector    -> Vector(it.getArray().copyOf().apply { sortWith(comparator) } )
                    is ByteArray -> it.copyOf().apply { sortedWith(comparator) }
                    is Map<*, *> -> TreeMap<Any?, Any?>(comparator).apply { putAll(it) }
                    else         -> throw WrongTypeException(name, "Collection of comparable elements", it)
                }
            } catch (e: ClassCastException) {
                throw WrongTypeException(name, "Collection of comparable elements", it)
            }
        }
    }
}
