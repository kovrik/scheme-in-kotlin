package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.math.Addition
import core.procedures.math.Multiplication
import core.procedures.math.trigonometry.Cos
import core.procedures.math.trigonometry.Sin
import core.scm.BigComplex
import core.scm.Type
import java.lang.NullPointerException

class MakePolar : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "make-polar"

    override fun apply2(arg1: Any?, arg2: Any?): Number? {
        if (arg1 == null) throw NullPointerException()
        if (arg2 == null) throw NullPointerException()
        /* (+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i)) */
        val m = arg1 as Number?
        val a = arg2 as Number?
        return Addition.add(Multiplication.apply(m, Cos.cos(a!!)), BigComplex.I.multiply(Sin.sin(a)).multiply(m))
    }
}
