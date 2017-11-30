package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.utils.Utils
import kotlin.math.cos
import kotlin.math.cosh
import kotlin.math.sin
import kotlin.math.sinh

class Cos : AFn<Number?, Number>(name = "cos", isPure = true, minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg!!) -> 1L
        arg is BigComplex   -> Cos.cos(arg)
        else                -> cos(arg.toDouble())
    }

    companion object {

        private val multiplication = Multiplication()

        fun cos(c: BigComplex): BigComplex {
            val x = c.re.toDouble()
            val y = c.im.toDouble()
            return BigComplex(multiplication(cos(x), cosh(y)), multiplication(-1.0, multiplication(sin(x), sinh(y))))
        }
    }
}
