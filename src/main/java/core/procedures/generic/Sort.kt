package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Vector

import java.util.*

class Sort : AFn(FnArgsBuilder().min(1).max(2).build()) {

    override val isPure = true
    override val name = "sort"

    override operator fun invoke(vararg args: Any?): Any? {
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
                        val copy = Arrays.copyOf(arg.getArray(), arg.size)
                        Arrays.sort(copy)
                        return Vector(*copy)
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
                    val copy = Arrays.copyOf(arg.getArray(), arg.size)
                    Arrays.sort(copy, comparator)
                    return Vector(*copy)
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
