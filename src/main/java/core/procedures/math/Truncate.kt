package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal

class Truncate : AFn(name = "truncate", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    override operator fun invoke(arg: Any?): Number? {
        if (arg is Double || arg is Float) {
            if ((arg as Number).toDouble() < 0) {
                return Math.ceil(arg.toDouble())
            } else {
                return Math.floor(arg.toDouble())
            }
        } else if (arg is BigDecimal) {
            return when {
                arg.signum() < 0 -> arg.setScale(0, BigDecimal.ROUND_UP)
                else             -> arg.setScale(0, BigDecimal.ROUND_DOWN)
            }
        } else if (arg is BigRatio) {
            return arg.truncate()
        }
        return arg as Number?
    }
}
