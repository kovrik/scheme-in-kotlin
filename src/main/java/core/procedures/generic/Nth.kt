package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class Nth : AFn<Any?, Any?>(name = "nth", isPure = true, minArgs = 2, maxArgs = 3) {

    private val count = Count()
    private val get = Get()

    override operator fun invoke(vararg args: Any?): Any? {
        val col = args[0]
        if (col is Map<*, *>) {
            throw UnsupportedOperationException("nth not supported on this type: ${col.javaClass}")
        }
        if (!Utils.isSeqable(col)) {
            throw IllegalArgumentException("don't know how to create Sequence from ${col?.javaClass}")
        }
        val index = args[1]
        if (!Utils.isInteger(index)) {
            throw WrongTypeException(name, Int::class.java, index)
        }
        val i = (args[1] as Number).toInt()
        val size = count(col)
        if (size <= i && args.size < 3) {
            throw IndexOutOfBoundsException("$name: value out of range: $i")
        }
        return get(*args)
    }
}
