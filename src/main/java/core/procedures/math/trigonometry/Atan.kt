package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Atan : AFn<Number?, Number>(name = "atan", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes =  arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is BigDecimal -> atan(arg)
        arg is BigInteger -> atan(arg)
        arg is BigComplex -> atan(arg)
        arg is BigRatio   -> atan(arg.toBigDecimal())
        else              -> Math.atan(arg!!.toDouble())
    }

    companion object {

        fun atan(bd: BigDecimal) = bd.toDouble().let {
            when {
                !it.isFinite() -> Double.NaN
                else           -> Math.atan(it)
            }
        }

        fun atan(bi: BigInteger) = bi.toDouble().let {
            when {
                !it.isFinite() -> Double.NaN
                else           -> Math.atan(it)
            }
        }

        fun atan(c: BigComplex): Number {
            val r = c.re
            val i = c.im
            val a = r.toDouble()
            if (!a.isFinite()) {
                return Double.NaN
            }
            val b = i.toDouble()
            if (!b.isFinite()) {
                return Double.NaN
            }

            val a2 = a * a
            val b2 = b * b
            val re: Double
            if (r.signum() == 0 && i > BigDecimal.ONE.negate() && i < BigDecimal.ONE) {
                /* when x = 0 and -1 < y < 1 */
                re = 0.0
            } else if (r.signum() == 0 && i.multiply(i) > BigDecimal.ONE) {
                /* when x = 0 and 1 < y^2
                 * re(arctan(x + iy)) = pi/2 */
                re = Math.PI / 2
            } else if (r.signum() > 0) {
                /* when x > 0
                 * re(arctan(x + iy)) = pi/4 - (1/2) arctan ( (1 - x^2 - y^2)/(2x) ) */
                re = Math.PI / 4 - 0.5 * Math.atan((1.0 - a2 - b2) / (2 * a))
            } else {
                /* when x < 0
                 * re(arctan(x + iy)) = -pi/4 - (1/2) arctan ( (1 - x^2 - y^2)/(2x) ) */
                re = -Math.PI / 4 - 0.5 * Math.atan((1.0 - a2 - b2) / (2 * a))
            }

            if (!re.isFinite()) {
                return Double.NaN
            }

            /* im(arctan(x + iy)) = -(1/4) ln ((1 - x^2 - y^2)^2 + (2x)^2) + (1/2) ln ((1 + y)^2 + x^2) */
            val im = -0.25 * Math.log((1.0 - a2 - b2) * (1.0 - a2 - b2) + 4 * a2) + 0.5 * Math.log((1 + b) * (1 + b) + a2)
            if (!im.isFinite()) {
                return Double.NaN
            }
            return BigComplex(re, im)
        }
    }
}
