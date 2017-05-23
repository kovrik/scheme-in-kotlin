package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.scm.Type

import java.math.BigDecimal
import java.math.BigInteger

class Ceiling : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.Real::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "ceiling"

    override fun apply1(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        when (arg) {
            is Long, is Int, is Short, is Byte, is BigInteger -> return arg as Number?
            is Double -> return Math.ceil((arg as Double?)!!)
            is Float -> return Math.ceil((arg as Float?)!!.toDouble())
            is BigDecimal -> return arg.setScale(0, BigDecimal.ROUND_UP)
            is BigRatio -> return arg.ceiling()
            else -> return Math.ceil((arg as Number).toDouble())
        }
    }
}
