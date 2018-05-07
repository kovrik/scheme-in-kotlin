package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex
import core.utils.Utils
import java.math.BigDecimal
import kotlin.math.sqrt

class Sqrt : AFn<Number?, Number>(name = "sqrt", isPure = true, minArgs = 1, maxArgs = 1,
                                  mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is BigComplex -> when (arg.im.signum()) {
            0    -> invoke(arg.re)
            else -> arg.sqrt()
        }
        is BigDecimal -> arg.sqrt(Utils.DEFAULT_CONTEXT)
        else -> sqrt(arg!!.toDouble())
    }
}
