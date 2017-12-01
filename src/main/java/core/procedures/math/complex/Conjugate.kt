package core.procedures.math.complex

import core.procedures.AFn
import core.scm.BigComplex

class Conjugate : AFn<Number?, Number?>(name = "conjugate", isPure = true, minArgs =  1, maxArgs = 1,
                                       mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when (arg) {
        is BigComplex -> BigComplex(arg.re, arg.im.negate())
        else -> arg
    }
}