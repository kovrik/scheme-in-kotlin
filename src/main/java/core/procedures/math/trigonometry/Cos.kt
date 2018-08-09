package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.math.Multiplication
import core.scm.Complex
import core.utils.Utils
import kotlin.math.cos
import kotlin.math.cosh
import kotlin.math.sin
import kotlin.math.sinh

class Cos : AFn<Number?, Number>(name = "cos", isPure = true, arity = Exactly(1),
                                 mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg!!) -> 1L
        arg is Complex   -> cos(arg)
        else                -> cos(arg.toDouble())
    }

    companion object {

        private val multiplication = Multiplication()

        fun cos(c: Complex): Complex {
            val x = c.re.toDouble()
            val y = c.im.toDouble()
            return Complex(multiplication.invoke(cos(x), cosh(y)), multiplication.invoke(-1.0, multiplication.invoke(sin(x), sinh(y))))
        }
    }
}
