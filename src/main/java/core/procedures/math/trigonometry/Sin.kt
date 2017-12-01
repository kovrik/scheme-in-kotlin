package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.utils.Utils
import kotlin.math.cos
import kotlin.math.cosh
import kotlin.math.sin
import kotlin.math.sinh

class Sin : AFn<Number?, Number>(name = "sin", isPure = true, minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg!!) -> 0L
        arg is BigComplex   -> sin(arg)
        else                -> sin(arg.toDouble())
    }

    companion object {

        private val multiplication = Multiplication()

        fun sin(c: BigComplex): BigComplex {
            val x = c.re.toDouble()
            val y = c.im.toDouble()
            return BigComplex(multiplication(sin(x), cosh(y)), multiplication(cos(x), sinh(y)))
        }
    }
}
