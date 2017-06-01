package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Remainder : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(Long::class.javaObjectType, Long::class.javaObjectType))) {

    companion object {

        private val NAME = "remainder"

        private operator fun invoke(first: BigDecimal, second: BigDecimal) = first.remainder(second)

        private operator fun invoke(first: BigInteger, second: BigInteger) = first.remainder(second)

        fun remainder(first: Number, second: Number): Number? {
            if (Utils.isZero(second)) {
                throw ArithmeticException("remainder: undefined for 0")
            }
            if (Utils.isZero(first)) {
                return Utils.inexactnessTaint(first, second)
            }
            if (first is BigRatio || second is BigRatio) {
                return invoke(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
            }
            if (first is BigDecimal || second is BigDecimal) {
                return invoke(Utils.toBigDecimal(first), Utils.toBigDecimal(second))
            }
            if (first is BigInteger || second is BigInteger) {
                return invoke(Utils.toBigInteger(first), Utils.toBigInteger(second))
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

    override val isPure = true
    override val name = NAME
    override operator fun invoke(arg1: Any?, arg2: Any?) = remainder(arg1!! as Number, arg2!! as Number)
}
