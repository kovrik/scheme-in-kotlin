package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableVector
import core.scm.Type

class VectorRef : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(MutableVector::class.java, Type.ExactNonNegativeInteger::class.java))) {

    override val isPure = true
    override val name = "vector-ref"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val vec = arg1 as MutableVector?
        val pos = (arg2 as Number).toLong()
        if (pos >= vec!!.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return vec[pos.toInt()]
    }
}
