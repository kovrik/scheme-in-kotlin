package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex
import core.utils.Utils
import kotlin.math.cos

import kotlin.math.cosh
import kotlin.math.sin
import kotlin.math.sinh

class Cosh : AFn<Number?, Number>(name = "cosh", isPure = true, arity = Exactly(1),
                                  mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 1L
        arg is Complex -> cosh(arg)
        else              -> cosh(arg!!.toDouble())
    }

    companion object {
        /* cosh(x + yi) = cosh(x)*cos(y) + sinh(x)*sin(y)*i */
        internal fun cosh(c: Complex): Number {
            val x = c.re.toDouble()
            val y = c.im.toDouble()
            val re = cosh(x) * cos(y)
            if (!re.isFinite()) {
                return re
            }
            val im = sinh(x) * sin(y)
            if (!im.isFinite()) {
                return im
            }
            return Complex(re, im)
        }
    }
}
