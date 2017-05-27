package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal

class Truncate : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.Real::class.java)).build()) {

    override val isPure = true
    override val name = "truncate"

    override operator fun invoke(arg: Any?): Number? {
        if (arg is Double || arg is Float) {
            if ((arg as Number).toDouble() < 0) {
                return Math.ceil(arg.toDouble())
            } else {
                return Math.floor(arg.toDouble())
            }
        } else if (arg is BigDecimal) {
            val bd = arg
            when {
                bd.signum() < 0 -> return bd.setScale(0, BigDecimal.ROUND_UP)
                else            -> return bd.setScale(0, BigDecimal.ROUND_DOWN)
            }
        } else if (arg is BigRatio) {
            return arg.truncate()
        }
        return arg as Number?
    }
}
