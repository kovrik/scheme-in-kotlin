package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import java.lang.NullPointerException

class RealPart : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure = true
    override val name = "real-part"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        if (arg is BigComplex) return arg.re
        return arg as Number?
    }
}
