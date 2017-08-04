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

    private val multiplication = Multiplication()

    /* (+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i)) */
    override operator fun invoke(arg1: Number?, arg2: Number?) = Addition.add(multiplication(arg1!!, Cos.cos(arg2!!)),
                                                                              BigComplex.I * Sin.sin(arg2) * arg1)!!
}
