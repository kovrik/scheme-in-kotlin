package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.math.Addition
import core.procedures.math.Multiplication
import core.procedures.math.trigonometry.Cos
import core.procedures.math.trigonometry.Sin
import core.scm.Complex
import core.scm.Type

class MakePolar : AFn<Number?, Number>(name = "make-polar", isPure = true, arity = Exactly(2),
                                       mandatoryArgsTypes = arrayOf(Type.Real::class.java, Type.Real::class.java)) {

    private val addition = Addition()
    private val multiplication = Multiplication()
    private val sin = Sin()
    private val cos = Cos()

    /* (+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i)) */
    override operator fun invoke(arg1: Number?, arg2: Number?) = addition(multiplication(arg1!!, cos(arg2!!)),
                                                                          Complex.I * sin(arg2) * arg1)
}
