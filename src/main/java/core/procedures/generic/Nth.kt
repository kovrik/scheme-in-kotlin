package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils

class Nth : AFn(FnArgsBuilder().min(2).max(3).build()) {

    private val count = Count()
    private val get = Get()

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "nth"

    override fun apply(args: Array<Any?>): Any? {
        val col = args[0]
        if (col is Map<*, *>) {
            throw UnsupportedOperationException("nth not supported on this type: " + col.javaClass)
        }
        if (!Utils.isSeqable(col)) {
            throw IllegalArgumentException("don't know how to create Sequence from " + col?.javaClass)
        }
        val index = args[1]
        if (!Utils.isInteger(index)) {
            throw WrongTypeException(name, Int::class.java, index)
        }
        val i = (args[1] as Number).toInt()
        val size = count.apply1(col)!!
        if (size <= i && args.size < 3) {
            throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", name, i))
        }
        return get.apply(args)
    }
}
