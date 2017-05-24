package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Denominator : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(BigRatio::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "denominator"

    override fun apply1(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        return denominator(arg)
    }

    private fun denominator(o: Any): Number {
        val isExact = Utils.isExact(o)
        val exact: Number
        if (isExact) {
            exact = o as Number
        } else {
            exact = ToExact.toExact(o)
        }
        if (exact is BigRatio) {
            if (!isExact) {
                val result = BigDecimal(exact.denominator)
                return result.setScale(1, Utils.ROUNDING_MODE)
            }
            return exact.denominator
        }
        if (exact is Long || exact is Int || exact is Byte || exact is Short) {
            return 1L
        }
        if (exact is Double || exact is Float) {
            return 1.0
        }
        if (exact is BigInteger) {
            return BigInteger.ONE
        }
        if (exact is BigDecimal) {
            when {
                exact.scale() == 0 -> return BigDecimal.ONE
                else -> return BigDecimal.ONE.setScale(1, Utils.ROUNDING_MODE)
            }
        }
        return 1L
    }
}