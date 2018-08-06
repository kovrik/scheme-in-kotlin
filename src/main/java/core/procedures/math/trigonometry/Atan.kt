package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex
import core.utils.Utils

import java.math.BigDecimal
import kotlin.math.PI
import kotlin.math.atan
import kotlin.math.ln

class Atan : AFn<Number?, Number>(name = "atan", isPure = true, arity = Exactly(1),
                                  mandatoryArgsTypes =  arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is Complex -> atan(arg)
        else              -> atan(arg!!.toDouble())
    }

    private fun atan(c: Complex): Number {
        val r = c.re
        val i = c.im
        val a = r.toDouble()
        if (!Utils.isFinite(a)) {
            return a
        }
        val b = i.toDouble()
        if (!Utils.isFinite(b)) {
            return b
        }
        val a2 = a * a
        val b2 = b * b
        val re = when {
            /* when x = 0 and -1 < y < 1 */
            r.signum() == 0 && i > -BigDecimal.ONE && i < BigDecimal.ONE -> 0.0
            /* when x = 0 and 1 < y^2
             * re(arctan(x + iy)) = pi/2 */
            r.signum() == 0 && i * i > BigDecimal.ONE -> PI / 2
            /* when x > 0
             * re(arctan(x + iy)) = pi/4 - (1/2) arctan ( (1 - x^2 - y^2)/(2x) ) */
            r.signum() > 0 -> PI / 4 - 0.5 * atan((1.0 - a2 - b2) / (2 * a))
            /* when x < 0
             * re(arctan(x + iy)) = -pi/4 - (1/2) arctan ( (1 - x^2 - y^2)/(2x) ) */
            else -> -PI / 4 - 0.5 * atan((1.0 - a2 - b2) / (2 * a))
        }

        if (!Utils.isFinite(re)) {
            return re
        }

        /* im(arctan(x + iy)) = -(1/4) ln ((1 - x^2 - y^2)^2 + (2x)^2) + (1/2) ln ((1 + y)^2 + x^2) */
        val im = -0.25 * ln((1.0 - a2 - b2) * (1.0 - a2 - b2) + 4 * a2) + 0.5 * ln((1 + b) * (1 + b) + a2)
        if (!Utils.isFinite(im)) {
            return im
        }
        return Complex(re, im)
    }
}
