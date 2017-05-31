package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex

class ImagPart : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "imag-part"

    override operator fun invoke(arg: Any?): Number? {
        arg!!
        if (arg is BigComplex) return arg.im
        return 0L
    }
}
