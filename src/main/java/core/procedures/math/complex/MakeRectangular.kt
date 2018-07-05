package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.BigComplex
import core.scm.Type

class MakeRectangular : AFn<Number?, Number>(name = "make-rectangular", isPure = true, arity = Exactly(2),
                            mandatoryArgsTypes = arrayOf(Type.Real::class.java, Type.Real::class.java)) {

    /* (+ x (* y 0+1i)) */
    override operator fun invoke(arg1: Number?, arg2: Number?) = (BigComplex.I * arg2!!) + arg1!!
}
