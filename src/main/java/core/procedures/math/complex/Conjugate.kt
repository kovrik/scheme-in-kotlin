package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex

class Conjugate : AFn<Number?, Number?>(name = "conjugate", isPure = true, arity = Exactly(1),
                                        mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when (arg) {
        is Complex -> Complex(arg.re, arg.im.negate())
        else -> arg
    }
}