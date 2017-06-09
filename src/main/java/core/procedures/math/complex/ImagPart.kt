package core.procedures.math.complex

import core.procedures.AFn
import core.scm.BigComplex

class ImagPart : AFn(name = "imag-part", isPure = true, minArgs = 1, maxArgs = 1,
                     mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?): Number? {
        arg!!
        if (arg is BigComplex) return arg.im
        return 0L
    }
}
