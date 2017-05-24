package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Subtraction : AFn(FnArgsBuilder().min(1).rest(Number::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "-"

    override fun apply(vararg args: Any?): Any? {
        if (args.size == 1) {
            when {
                args[0] is BigDecimal -> return (args[0] as BigDecimal).negate()
                args[0] is BigInteger -> return (args[0] as BigInteger).negate()
                args[0] is BigRatio -> return (args[0] as BigRatio).negate()
                args[0] is Long -> try {
                    return Math.negateExact(args[0] as Long)
                } catch (e: ArithmeticException) {
                    return BigInteger.valueOf(args[0] as Long).negate()
                }
            }
            if (args[0] is Int) {
                try {
                    return Math.negateExact(args[0] as Int)
                } catch (e: ArithmeticException) {
                    return Math.negateExact((args[0] as Int).toLong())
                }

            }
            return subtract(0L, args[0] as Number)
        }
        var result = args[0]
        for (i in 1..args.size - 1) {
            result = subtract(result as Number, args[i] as Number)
        }
        return result
    }

    private fun subtract(first: Number, second: Number): Number? {
        var first = first
        var second = second
        /* Special cases */
        if (Utils.isZero(second)) {
            return Utils.inexactnessTaint(first, second)
        }
        /* Complex numbers*/
        if (first is BigComplex) {
            return first.minus(second)
        }
        if (second is BigComplex) {
            return BigComplex(first).minus(second)
        }
        /* Big Ratio numbers */
        if (first is BigRatio && second is BigRatio) {
            return first.minus(second)
        }
        if (first is BigRatio) {
            if (Utils.isExact(second)) {
                return first.minus(BigRatio.valueOf(second.toString(), "1"))
            } else {
                first = first.toDouble()
            }
        }
        if (second is BigRatio) {
            if (Utils.isExact(first)) {
                return BigRatio.valueOf(first.toString(), "1").minus(second)
            } else {
                second = second.toDouble()
            }
        }
        if (first is Float && second is Float) {
            val result = first.toFloat() - second.toFloat()
            if (java.lang.Float.isNaN(result) || java.lang.Float.isInfinite(result)) {
                return Utils.toBigDecimal(first).subtract(Utils.toBigDecimal(second))
            }
            return result
        }
        if (first is Double || second is Double || first is Float || second is Float) {
            val result = first.toDouble() - second.toDouble()
            if (java.lang.Double.isNaN(result) || java.lang.Double.isInfinite(result)) {
                return Utils.toBigDecimal(first).subtract(Utils.toBigDecimal(second))
            }
            return result
        }
        if (first is BigDecimal || second is BigDecimal) {
            return Utils.toBigDecimal(first).subtract(Utils.toBigDecimal(second))
        }
        if (first is BigInteger || second is BigInteger) {
            return Utils.toBigInteger(first).subtract(Utils.toBigInteger(second))
        }
        val f = first.toLong()
        val s = second.toLong()
        try {
            return Math.subtractExact(f, s)
        } catch (e: ArithmeticException) {
            return BigDecimal.valueOf(f).subtract(BigDecimal.valueOf(s))
        }
    }
}
