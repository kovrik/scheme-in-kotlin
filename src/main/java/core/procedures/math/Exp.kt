package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.utils.Utils
import java.lang.NullPointerException

class Exp : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "exp"

    override fun apply1(arg: Any?): Number? {
        return exp(arg as Number?)
    }

    companion object {

        fun exp(number: Number?): Number? {
            if (number == null) throw NullPointerException()
            if (number is Double) {
                if (number == java.lang.Double.NEGATIVE_INFINITY) {
                    return 0L
                }
                if (java.lang.Double.isNaN(number) || java.lang.Double.isInfinite(number)) {
                    return number
                }
                return Math.exp(number.toDouble())
            }
            if (number is Float) {
                if (number == java.lang.Float.NEGATIVE_INFINITY) {
                    return 0L
                }
                if (java.lang.Float.isNaN(number) || java.lang.Float.isInfinite(number)) {
                    return number
                }
                return Math.exp(number.toDouble())
            }
            if (number is Long || number is Byte || number is Short || number is Int) {
                if (number.toLong() == 0L) {
                    return 1L
                }
                return Math.exp(number.toDouble())
            }
            if (number is BigRatio) {
                /* Special cases */
                if (number.isZero) {
                    return 1L
                }
                if (number.isOne) {
                    return Math.exp(1.0)
                }
            }
            return Expt.expt(Utils.E, number)
        }
    }
}
