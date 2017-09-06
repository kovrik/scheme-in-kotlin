package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal

class Truncate : AFn<Number?, Number>(name = "truncate", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Double, is Float -> when {
            arg.toDouble() < 0 -> Math.ceil(arg.toDouble())
            else               -> Math.floor(arg.toDouble())
        }
        is BigDecimal -> when {
            arg.signum() < 0 -> arg.setScale(0, BigDecimal.ROUND_UP)
            else             -> arg.setScale(0, BigDecimal.ROUND_DOWN)
        }
        is BigRatio -> arg.truncate()
        else -> arg!!
    }
}
