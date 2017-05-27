package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type
import core.scm.MutableVector
import core.scm.Void

class VectorSet : AFn(FnArgsBuilder().min(3).max(3)
        .mandatory(arrayOf(MutableVector::class.java, Type.ExactNonNegativeInteger::class.java)).build()) {

    override val name = "vector-set!"

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        val vec = arg1 as MutableVector?
        val pos = (arg2 as Number).toLong()
        if (pos >= vec!!.size) {
            throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", name, pos))
        }
        vec[pos.toInt()] = arg3
        return Void
    }
}
