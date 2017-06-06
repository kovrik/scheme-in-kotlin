package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.Type

class MakeRectangular : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java))) {

    override val isPure = true
    override val name = "make-rectangular"

    override operator fun invoke(arg1: Any?, arg2: Any?): Number {
        /* (+ x (* y 0+1i)) */
        val x = arg1!! as Number
        val y = arg2!! as Number
        return (BigComplex.I * y) + x
    }
}
