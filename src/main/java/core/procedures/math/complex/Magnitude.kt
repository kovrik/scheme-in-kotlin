package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.math.Abs
import core.scm.BigComplex

class Magnitude : AFn(name = "magnitude", isPure = true, minArgs = 1, maxArgs = 1,
                      mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?): Number? {
        arg!!
        if (arg is BigComplex) return arg.magnitude()
        return Abs.abs((arg as Number?)!!)
    }
}
