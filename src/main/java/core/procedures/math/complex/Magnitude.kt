package core.procedures.math.complex

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.math.Abs
import core.scm.BigComplex
import java.lang.NullPointerException

class Magnitude : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "magnitude"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        if (arg is BigComplex) {
            return arg.magnitude()
        }
        return Abs.abs((arg as Number?)!!)
    }
}
