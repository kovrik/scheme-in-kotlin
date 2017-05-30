package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal
import java.math.BigInteger

class Floor : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.Real::class.java))) {

    override val isPure = true
    override val name = "floor"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        return when (arg) {
            is Long, is Int, is Short, is Byte, is BigInteger -> arg as Number?
            is Double -> Math.floor((arg as Double?)!!)
            is Float -> Math.floor((arg as Float?)!!.toDouble())
            is BigDecimal -> arg.setScale(0, BigDecimal.ROUND_DOWN)
            is BigRatio -> arg.floor()
            else -> Math.floor((arg as Number).toDouble())
        }
    }
}
