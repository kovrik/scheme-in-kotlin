package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.Type
import java.lang.NullPointerException

class MakeRectangular : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "make-rectangular"

    override fun apply2(arg1: Any?, arg2: Any?): Number {
        if (arg1 == null) throw NullPointerException()
        if (arg2 == null) throw NullPointerException()
        /* (+ x (* y 0+1i)) */
        val x = arg1 as Number
        val y = arg2 as Number
        return BigComplex.I.multiply(y).plus(x)
    }
}
