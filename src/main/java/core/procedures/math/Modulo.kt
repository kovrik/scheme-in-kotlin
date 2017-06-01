package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

open class Modulo : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(Long::class.javaObjectType, Long::class.javaObjectType))) {

    companion object {
        private val REM = Remainder()
    }

    override val isPure = true
    override val name = "modulo"

    override operator fun invoke(arg1: Any?, arg2: Any?): Number? {
        arg1!!
        arg2!!
        if (Utils.isZero(arg2)) throw ArithmeticException("modulo: undefined for 0")
        return invoke(arg1 as Number, arg2 as Number)
    }

    private operator fun invoke(first: BigDecimal, second: BigDecimal): BigDecimal {
        val remainder = first.remainder(second)
        if (remainder.signum() == 0) {
            return remainder
        }
        if (first.signum() > 0 == second.signum() > 0) {
            return remainder
        }
        return second.add(remainder)
    }

    private operator fun invoke(first: BigInteger, second: BigInteger): BigInteger {
        val remainder = first.remainder(second)
        if (remainder.signum() == 0) {
            return remainder
        }
        if (first.signum() > 0 == second.signum() > 0) {
            return remainder
        }
        return second.add(remainder)
    }

    private operator fun invoke(first: Number, second: Number): Number? {
        if (Utils.isZero(first)) {
            return Utils.inexactnessTaint(first, second)
        }
        if (first is BigDecimal || second is BigDecimal) {
            return invoke(Utils.toBigDecimal(first), second as BigDecimal)
        }
        if (first is BigInteger || second is BigInteger) {
            return invoke(Utils.toBigInteger(first), Utils.toBigInteger(second))
        }
        val m = REM(first, second)
        if (m!!.toInt() == 0) {
            return m
        }
        if (first.toLong() > 0 == second.toLong() > 0) {
            return m
        }
        if (first is Double || second is Double || first is Float || second is Float) {
            return m.toDouble() + second.toDouble()
        }
        return m.toLong() + second.toLong()
    }
}
