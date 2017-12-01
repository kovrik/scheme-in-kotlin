package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.utils.Utils
import kotlin.math.cos

import kotlin.math.cosh
import kotlin.math.sin
import kotlin.math.sinh

class Cosh : AFn<Number?, Number>(name = "cosh", isPure = true, minArgs = 1, maxArgs = 1,
                                  mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 1L
        arg is BigComplex -> cosh(arg)
        else              -> cosh(arg!!.toDouble())
    }

    companion object {
        /* cosh(x + yi) = cosh(x)*cos(y) + sinh(x)*sin(y)*i */
        internal fun cosh(c: BigComplex): Number {
            val x = c.re.toDouble()
            val y = c.im.toDouble()
            val re = cosh(x) * cos(y)
            val im = sinh(x) * sin(y)
            return when {
                !re.isFinite() -> Double.NaN
                !im.isFinite() -> Double.NaN
                else -> BigComplex(re, im)
            }
        }
    }
}
