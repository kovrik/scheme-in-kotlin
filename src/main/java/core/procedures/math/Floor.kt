package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal
import java.math.BigInteger

class Floor : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.Real::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "floor"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        when (arg) {
            is Long, is Int, is Short, is Byte, is BigInteger -> return arg as Number?
            is Double -> return Math.floor((arg as Double?)!!)
            is Float -> return Math.floor((arg as Float?)!!.toDouble())
            is BigDecimal -> return arg.setScale(0, BigDecimal.ROUND_DOWN)
            is BigRatio -> return arg.floor()
            else -> return Math.floor((arg as Number).toDouble())
        }
    }
}
