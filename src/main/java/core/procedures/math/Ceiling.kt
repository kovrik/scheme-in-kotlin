package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal
import java.math.BigInteger

class Ceiling : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.Real::class.java))) {

    override val isPure = true
    override val name = "ceiling"

    override operator fun invoke(arg: Any?): Number? {
        arg!!
        return when (arg) {
            is Long, is Int, is Short, is Byte, is BigInteger -> arg as Number?
            is Double -> Math.ceil((arg as Double?)!!)
            is Float -> Math.ceil((arg as Float?)!!.toDouble())
            is BigDecimal -> arg.setScale(0, BigDecimal.ROUND_UP)
            is BigRatio -> arg.ceiling()
            else -> Math.ceil((arg as Number).toDouble())
        }
    }
}
