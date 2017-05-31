package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Addition : AFn(FnArgs(rest = Number::class.java)) {

    override val isPure = true
    override val name = "+"

    override operator fun invoke(vararg args: Any?): Number? {
        when (args.size) {
            0 -> return 0L
            1 -> return args[0] as Number?
            else -> {
                var result: Any? = 0L
                for (arg in args) {
                    if (arg == null) throw NullPointerException()
                    result = add(result as Number, arg as Number)
                }
                return result as Number
            }
        }
    }

    companion object {

        fun add(first: Number?, second: Number?): Number? {
            if (first  == null) throw NullPointerException()
            if (second == null) throw NullPointerException()
            var first = first
            var second = second
            /* Special cases */
            if (Utils.isZero(first)) {
                return Utils.inexactnessTaint(second, first)
            }
            if (Utils.isZero(second)) {
                return Utils.inexactnessTaint(first, second)
            }
            /* Complex numbers*/
            if (first is BigComplex) {
                return first.plus(second)
            }
            if (second is BigComplex) {
                return second.plus(first)
            }
            /* Big Ratio numbers */
            if (first is BigRatio && second is BigRatio) {
                return first.plus(second)
            }
            if (first is BigRatio) {
                if (Utils.isExact(second)) {
                    return first.plus(BigRatio.valueOf(second.toString(), "1"))
                } else {
                    first = first.toDouble()
                }
            }
            if (second is BigRatio) {
                if (Utils.isExact(first)) {
                    return BigRatio.valueOf(first.toString(), "1").plus(second)
                } else {
                    second = second.toDouble()
                }
            }
            if (first is Float && second is Float) {
                val result = first.toFloat() + second.toFloat()
                if (!result.isFinite()) {
                    return Utils.toBigDecimal(first).add(Utils.toBigDecimal(second))
                }
                return result
            }
            if (first is Double || second is Double || first is Float || second is Float) {
                val result = first.toDouble() + second.toDouble()
                if (!result.isFinite()) {
                    return Utils.toBigDecimal(first).add(Utils.toBigDecimal(second))
                }
                return result
            }
            if (first is BigDecimal || second is BigDecimal) {
                return Utils.toBigDecimal(first).add(Utils.toBigDecimal(second))
            }
            if (first is BigInteger || second is BigInteger) {
                return Utils.toBigInteger(first).add(Utils.toBigInteger(second))
            }
            val f = first.toLong()
            val s = second.toLong()
            try {
                return Math.addExact(f, s)
            } catch (e: ArithmeticException) {
                return BigDecimal.valueOf(f).add(BigDecimal.valueOf(s))
            }
        }
    }
}
