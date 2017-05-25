package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableVector
import core.scm.Void

import java.util.Arrays

class VectorFill : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(MutableVector::class.java, Any::class.java)).build()) {

    override val name: String
        get() = "vector-fill!"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val vector = arg1 as MutableVector?
        Arrays.fill(vector!!.getArray(), arg2)
        return Void.VOID
    }
}
