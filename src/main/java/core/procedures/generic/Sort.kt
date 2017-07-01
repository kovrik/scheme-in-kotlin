package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Vector
import java.util.*

class Sort : AFn<Any?, Any?>(name = "sort", isPure = true, minArgs = 1, maxArgs = 2) {

    override operator fun invoke(args: Array<Any?>): Any? {
        if (args.size == 1 || args[0] == null) {
            val arg = args[0] ?: args[1]
            try {
                when (arg) {
                    is List<*> -> {
                        val copy = mutableListOf<Comparable<Any?>>()
                        copy.addAll((arg as List<Comparable<Any?>>))
                        copy.sort()
                        return copy
                    }
                    is Vector -> {
                        val copy = arg.getArray().copyOf()
                        copy.sort()
                        return Vector(copy)
                    }
                    is Map<*, *> -> return TreeMap(arg as Map<*, *>?)
                }
            } catch (e: ClassCastException) {
                // ignore
            }
            throw WrongTypeException(name, "Collection of comparable elements", arg)
        }
        val comparator: Comparator<Any?> = args[0] as Comparator<Any?>
        val arg = args[1]
        try {
            when (arg) {
                is List<*> -> {
                    val copy = mutableListOf<Comparable<Any?>>()
                    copy.addAll((arg as List<Comparable<Any?>>))
                    copy.sortWith(comparator)
                    return copy
                }
                is Vector -> {
                    val copy = arg.getArray().copyOf()
                    copy.sortWith(comparator)
                    return Vector(copy)
                }
                is Map<*, *> -> {
                    val treeMap = TreeMap<Any?, Any?>(comparator)
                    treeMap.putAll(arg)
                    return treeMap
                }
            }
        } catch (e: ClassCastException) {
            // ignore
        }
        throw WrongTypeException(name, "Collection of comparable elements", arg)
    }
}
