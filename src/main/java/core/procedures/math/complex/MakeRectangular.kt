package core.procedures.math.complex

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.Type

class MakeRectangular : AFn(name = "make-rectangular", isPure = true, minArgs = 2, maxArgs = 2,
                            mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Number {
        /* (+ x (* y 0+1i)) */
        val x = arg1!! as Number
        val y = arg2!! as Number
        return (BigComplex.I * y) + x
    }
}
