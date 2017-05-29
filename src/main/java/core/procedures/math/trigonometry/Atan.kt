package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.lang.NullPointerException
import java.math.BigDecimal
import java.math.BigInteger

class Atan : AFn(FnArgs(min = 1, max = 1, mandatory =  arrayOf<Class<*>>(Number::class.java))) {

    override val isPure = true
    override val name = "atan"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        when {
            Utils.isZero(arg) -> return 0L
            arg is BigDecimal -> return atan(arg)
            arg is BigInteger -> return atan(arg)
            arg is BigComplex -> return atan(arg)
            arg is BigRatio   -> return atan(arg.toBigDecimal())
            else -> return Math.atan((arg as Number).toDouble())
        }
    }

    companion object {

        fun atan(bd: BigDecimal): Double {
            val v = bd.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.atan(v)
            }
        }

        fun atan(bi: BigInteger): Double {
            val v = bi.toDouble()
            when {
                java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v) -> return java.lang.Double.NaN
                else -> return Math.atan(v)
            }
        }

        fun atan(c: BigComplex): Number {
            val r = c.re
            val i = c.im
            val a = r.toDouble()
            if (java.lang.Double.isInfinite(a) || java.lang.Double.isNaN(a)) {
                return java.lang.Double.NaN
            }
            val b = i.toDouble()
            if (java.lang.Double.isInfinite(b) || java.lang.Double.isNaN(b)) {
                return java.lang.Double.NaN
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

            if (java.lang.Double.isInfinite(re) || java.lang.Double.isNaN(re)) {
                return java.lang.Double.NaN
            }

            /* im(arctan(x + iy)) = -(1/4) ln ((1 - x^2 - y^2)^2 + (2x)^2) + (1/2) ln ((1 + y)^2 + x^2) */
            val im = -0.25 * Math.log((1.0 - a2 - b2) * (1.0 - a2 - b2) + 4 * a2) + 0.5 * Math.log((1 + b) * (1 + b) + a2)
            if (java.lang.Double.isInfinite(im) || java.lang.Double.isNaN(im)) {
                return java.lang.Double.NaN
            }
            return BigComplex(re, im)
        }
    }
}
