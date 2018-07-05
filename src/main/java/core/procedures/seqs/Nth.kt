package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.Type
import core.utils.Utils

class Nth : AFn<Any?, Any?>(name = "nth", isPure = true, arity = Range(2, 3)) {

    private val count = Count()
    private val get = Get()

    override operator fun invoke(args: Array<out Any?>): Any? {
        val col = args[0]
        if (col is Map<*, *>) {
            throw UnsupportedOperationException("$name: not supported on this type: ${col.javaClass}")
        }
        if (!Utils.isSeqable(col)) {
            throw IllegalArgumentException("$name: don't know how to create Sequence from ${col?.javaClass}")
        }
        val index = args[1]
        Type.assertType(name, index, Int::class.java)
        val i = (index as Number).toInt()
        if (col !is Sequence<*> && count(col) <= i && args.size < 3) {
            throw IndexOutOfBoundsException("$name: value out of range: $i")
        }
        return get(args)
    }
}
