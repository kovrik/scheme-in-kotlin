package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.math.Addition
import core.procedures.math.Multiplication
import core.procedures.math.trigonometry.Cos
import core.procedures.math.trigonometry.Sin
import core.scm.BigComplex
import core.scm.Type

class MakePolar : AFn<Number?, Number>(name = "make-polar", isPure = true, minArgs = 2, maxArgs = 2,
                      mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java)) {

    override operator fun invoke(arg1: Number?, arg2: Number?): Number {
        /* (+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i)) */
        val m = arg1!!
        val a = arg2!!
        return Addition.add(Multiplication(m, Cos.cos(a)), BigComplex.I * Sin.sin(a) * m)!!
    }
}
