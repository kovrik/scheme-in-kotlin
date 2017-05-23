package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Vector

import java.util.*

class Sort : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "sort"

    // TODO accept comparator as optional first argument
    override fun apply1(arg: Any?): Any? {
        try {
            if (arg is List<*>) {
                Collections.sort<Comparable<Any?>>((arg as List<Comparable<Any?>>?)!!)
                return arg
            }
            if (arg is Vector) {
                Arrays.sort(arg.getArray())
                return arg
            }
            if (arg is Map<*, *>) {
                return TreeMap(arg as Map<*, *>?)
            }
        } catch (e: ClassCastException) {
            // ignore
        }
        throw WrongTypeException(name, "Collection of comparable elements", arg)
    }
}
