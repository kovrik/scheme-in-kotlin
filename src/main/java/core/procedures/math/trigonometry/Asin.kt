package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.lang.NullPointerException
import java.math.BigDecimal
import java.math.BigInteger

class Asin : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "asin"

    override fun apply1(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        /* Special cases */
        when {
            Utils.isZero(arg) -> return 0L
            arg is BigDecimal -> return asin(arg)
            arg is BigInteger -> return asin(arg)
            arg is BigComplex -> return asin(arg)
            arg is BigRatio   -> return asin(arg.toBigDecimal())
            else -> {
                val asin = Math.asin((arg as Number).toDouble())
                if (java.lang.Double.isNaN(asin)) {
                    return asin(BigComplex(arg))
                }
                return asin
            }
        }
    }

    private fun asin(bd: BigDecimal): Number {
        val v = bd.toDouble()
        if (java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v)) {
            return java.lang.Double.NaN
        } else {
            val asin = Math.asin(v)
            if (java.lang.Double.isNaN(asin)) {
                return asin(BigComplex(bd))
            }
            return asin
        }
    }

    private fun asin(bi: BigInteger): Number {
        val v = bi.toDouble()
        if (java.lang.Double.isInfinite(v) || java.lang.Double.isNaN(v)) {
            return java.lang.Double.NaN
        } else {
            val asin = Math.asin(v)
            if (java.lang.Double.isNaN(asin)) {
                return asin(BigComplex(bi))
            }
            return asin
        }
    }

    /* asin(a+bi) = asin(A) + ln(B + sqrt(B*B - 1))*i
     *
     * A = (sqrt((1+a)^2 + b^2) - sqrt((1-a)^2 + b^2))/2
     * B = (sqrt((1+a)^2 + b^2) + sqrt((1-a)^2 + b^2))/2
     *
     **/
    private fun asin(c: BigComplex): Number {
        val r = c.re
        val i = c.im
        val signum: Int
        if (i.signum() == 0) {
            signum = -r.signum()
        } else {
            signum = i.signum()
        }
        val a = r.toDouble()
        if (java.lang.Double.isInfinite(a) || java.lang.Double.isNaN(a)) {
            return java.lang.Double.NaN
        }
        val b = i.toDouble()
        if (java.lang.Double.isInfinite(b) || java.lang.Double.isNaN(b)) {
            return java.lang.Double.NaN
        }

        val b2 = b * b
        val L = Math.sqrt((1 + a) * (1 + a) + b2)
        val R = Math.sqrt((1 - a) * (1 - a) + b2)
        val A = (L - R) / 2
        val B = (L + R) / 2

        val re = Math.asin(A)
        if (java.lang.Double.isInfinite(re) || java.lang.Double.isNaN(re)) {
            return java.lang.Double.NaN
        }

        val im = Math.log(B + Math.sqrt(B * B - 1))
        if (java.lang.Double.isInfinite(im) || java.lang.Double.isNaN(im)) {
            return java.lang.Double.NaN
        }
        return BigComplex(re, signum * im)
    }
}
