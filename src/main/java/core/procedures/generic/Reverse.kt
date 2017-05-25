package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.MapEntry
import core.scm.MutableVector
import core.scm.Vector

class Reverse : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val name: String
        get() = "reverse"

    override operator fun invoke(arg: Any?): Any? {
        when (arg) {
            is List<*> -> {
                val result = Cons.list<Any>()
                for (o in (arg as List<*>?)!!) {
                    result.push(o)
                }
                return result
            }
            is Set<*> -> return Cons.list(arg as Collection<Any?>)
            is Map.Entry<*, *> -> return MapEntry(arg.value, arg.key)
            is Vector -> {
                val array = arg.getArray()
                val reversed = MutableVector(array.size, null)
                for (i in array.indices) {
                    reversed.array[i] = array[array.size - i - 1]
                }
                return reversed
            }
            is CharSequence -> return StringBuilder((arg as CharSequence?)!!).reverse().toString()
            else -> throw WrongTypeException(name, "List or Vector or Set or String", arg)
        }
    }
}
