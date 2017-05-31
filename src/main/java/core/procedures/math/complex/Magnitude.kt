package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.math.Abs
import core.scm.BigComplex

class Magnitude : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "magnitude"

    override operator fun invoke(arg: Any?): Number? {
        arg!!
        if (arg is BigComplex) return arg.magnitude()
        return Abs.abs((arg as Number?)!!)
    }
}
