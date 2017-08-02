package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.scm.BigRatio
import java.math.BigDecimal
import java.math.BigInteger

class Acos : AFn<Number?, Number>(name = "acos", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when (arg) {
        is BigDecimal -> acos(arg)
        is BigInteger -> acos(arg)
        is BigComplex -> acos(arg)
        is BigRatio   -> acos(arg.toBigDecimal())
        else -> {
            val acos = Math.acos(arg!!.toDouble())
            when {
                acos.isNaN() -> acos(BigComplex(arg))
                else -> acos
            }
        }
    }

    private fun acos(bd: BigDecimal): Number {
        val v = bd.toDouble()
        if (!v.isFinite()) {
            return Double.NaN
        } else {
            val acos = Math.acos(v)
            if (acos.isNaN()) {
                return acos(BigComplex(bd))
            }
            return acos
        }
    }

    private fun acos(bi: BigInteger): Number {
        val v = bi.toDouble()
        if (!v.isFinite()) {
            return Double.NaN
        } else {
            val acos = Math.acos(v)
            if (acos.isNaN()) {
                return acos(BigComplex(bi))
            }
            return acos
        }
    }

    /* acos(a+bi) = acos(A) - ln(B + sqrt(B*B - 1))*i
     *
     * A = (sqrt((1+a)^2 + b^2) - sqrt((1-a)^2 + b^2))/2
     * B = (sqrt((1+a)^2 + b^2) + sqrt((1-a)^2 + b^2))/2
     **/
    private fun acos(c: BigComplex): Number {
        val r = c.re
        val i = c.im
        val signum: Int
        if (i.signum() == 0) {
            signum = r.signum()
        } else {
            signum = -i.signum()
        }
        val a = r.toDouble()
        if (!a.isFinite()) {
            return Double.NaN
        }
        val b = i.toDouble()
        if (!b.isFinite()) {
            return Double.NaN
        }

        val b2 = b * b
        val L = Math.sqrt((1 + a) * (1 + a) + b2)
        val R = Math.sqrt((1 - a) * (1 - a) + b2)
        val A = (L - R) / 2
        val B = (L + R) / 2

        val re = Math.acos(A)
        if (!re.isFinite()) {
            return Double.NaN
        }

        val im = Math.log(B + Math.sqrt(B * B - 1))
        if (!im.isFinite()) {
            return Double.NaN
        }
        return BigComplex(re, signum * im)
    }
}
