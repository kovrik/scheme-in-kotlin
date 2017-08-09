package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.MapEntry
import core.scm.MutableVector
import core.scm.Vector

class Reverse : AFn<Any?, Any?>(name = "reverse", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        is List<*>         -> Cons.list<Any>().apply { for (o in (arg as List<*>?)!!) { add(0, o) } }
        is Set<*>          -> Cons.list(arg as Collection<Any?>)
        is Map.Entry<*, *> -> MapEntry(arg.value, arg.key)
        is CharSequence    -> StringBuilder((arg as CharSequence?)!!).reverse().toString()
        is ByteArray       -> arg.copyOf().apply { reverse() }
        is Vector -> {
            val array = arg.getArray()
            MutableVector(array.size, null).apply {
                for (i in array.indices) {
                    this.array[i] = array[array.size - i - 1]
                }
            }
        }
        else -> throw WrongTypeException(name, "List or Vector or Set or String", arg)
    }
}
