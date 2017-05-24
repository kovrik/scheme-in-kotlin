package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

open class Quotient : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(Long::class.javaObjectType, Long::class.javaObjectType)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "quotient"

    override fun apply2(arg1: Any?, arg2: Any?): Number? {
        if (arg1 == null) throw NullPointerException()
        if (arg2 == null) throw NullPointerException()
        /* Special cases */
        if (Utils.isOne(arg2)) {
            return Utils.inexactnessTaint(arg1 as Number, arg2 as Number?)
        }
        if (Utils.isZero(arg2)) {
            throw ArithmeticException("quotient: undefined for 0")
        }
        return apply(arg1 as Number, arg2 as Number)
    }

    private fun apply(first: BigDecimal, second: BigDecimal): Number {
        val scale = Math.max(first.scale(), second.scale())
        if (scale > 0) {
            return first.divide(second, Utils.DEFAULT_CONTEXT).setScale(0, Utils.ROUNDING_MODE)
                    .setScale(1, Utils.ROUNDING_MODE)
        } else {
            return first.divideToIntegralValue(second).setScale(scale, Utils.ROUNDING_MODE)
        }
    }

    private fun apply(first: BigInteger, second: BigInteger): Number {
        return first.divide(second)
    }

    private fun apply(first: Number, second: Number): Number? {
        if (Utils.isZero(first)) {
            return Utils.inexactnessTaint(first, second)
        }
        if (first is BigDecimal || second is BigDecimal) {
            return apply(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
        }
        if (first is BigInteger || second is BigInteger) {
            return apply(Utils.toBigInteger(first), Utils.toBigInteger(second))
        }
        if ((first is Double || second is Double || first is Float || second is Float) &&
                Utils.isInteger(first) && Utils.isInteger(second)) {

            return java.lang.Long.valueOf(first.toLong() / second.toLong())!!.toDouble()
        }
        if (first is Double || second is Double ||
                first is BigRatio || second is BigRatio) {

            return apply(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
        }
        return first.toLong() / second.toLong()
    }
}