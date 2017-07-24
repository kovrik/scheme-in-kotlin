package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Vector
import java.util.*

class Sort : AFn<Any?, Any?>(name = "sort", isPure = true, minArgs = 1, maxArgs = 2) {

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.size == 1 || args[0] == null) {
            val arg = args[0] ?: args[1]
            try {
                return when (arg) {
                    is List<*>   -> (arg as List<Comparable<Any?>>).toMutableList().apply { sort() }
                    is Vector    -> Vector(arg.getArray().copyOf().apply { sort() })
                    is Map<*, *> -> TreeMap(arg as Map<*, *>?)
                    else         -> throw WrongTypeException(name, "Collection of comparable elements", arg)
                }
            } catch (e: ClassCastException) {
                // ignore
            }
            throw WrongTypeException(name, "Collection of comparable elements", arg)
        }
        val comparator: Comparator<Any?> = args[0] as Comparator<Any?>
        val arg = args[1]
        try {
            return when (arg) {
                is List<*>   -> (arg as List<Comparable<Any?>>).toMutableList().apply { sortWith(comparator) }
                is Vector    -> Vector(arg.getArray().copyOf().apply { sortWith(comparator) } )
                is Map<*, *> -> TreeMap<Any?, Any?>(comparator).apply { putAll(arg) }
                else         -> throw WrongTypeException(name, "Collection of comparable elements", arg)
            }
        } catch (e: ClassCastException) {
            // ignore
        }
        throw WrongTypeException(name, "Collection of comparable elements", arg)
    }
}
