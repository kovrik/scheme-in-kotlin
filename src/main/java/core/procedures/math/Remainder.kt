package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Remainder : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(Long::class.javaObjectType, Long::class.javaObjectType)).build()) {

    companion object {

        private val NAME = "remainder"

        private fun apply(first: BigDecimal, second: BigDecimal): Number {
            return first.remainder(second)
        }

        private fun apply(first: BigInteger, second: BigInteger): Number {
            return first.remainder(second)
        }

        fun remainder(first: Number, second: Number): Number? {
            if (Utils.isZero(second)) {
                throw ArithmeticException("remainder: undefined for 0")
            }
            if (Utils.isZero(first)) {
                return Utils.inexactnessTaint(first, second)
            }
            if (first is BigRatio || second is BigRatio) {
                return apply(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
            }
            if (first is BigDecimal || second is BigDecimal) {
                return apply(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
            }
            if (first is BigInteger || second is BigInteger) {
                return apply(Utils.toBigInteger(first), Utils.toBigInteger(second))
            }
            if (first is Double || second is Double || first is Float || second is Float) {
                val result = first.toDouble() % second.toDouble()
                // Don't want negative zero
                if (result == -0.0) {
                    return Math.abs(result)
                }
                return result
            }
            return first.toLong() % second.toLong()
        }
    }

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = NAME

    override fun apply2(arg1: Any?, arg2: Any?): Number? {
        if (arg1 == null) throw NullPointerException()
        if (arg2 == null) throw NullPointerException()
        return remainder(arg1 as Number, arg2 as Number)
    }
}
