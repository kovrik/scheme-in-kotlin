package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.math.Multiplication
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Sin : AFn<Number?, Number>(name = "sin", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = sin(arg!!)

    companion object {

        fun sin(n: Number) = when {
            Utils.isZero(n) -> 0L
            n is BigDecimal -> sin(n)
            n is BigInteger -> sin(n)
            n is BigComplex -> sin(n)
            n is BigRatio   -> sin(n.toBigDecimal())
            else            -> Math.sin(n.toDouble())
        }

        fun sin(bd: BigDecimal) = bd.toDouble().let {
            when {
                !it.isFinite() -> Double.NaN
                else           -> Math.sin(it)
            }
        }

        fun sin(bi: BigInteger) = bi.toDouble().let {
            when {
                !it.isFinite() -> Double.NaN
                else           -> Math.sin(it)
            }
        }

        fun sin(c: BigComplex) = BigComplex(Multiplication(Sin.sin(c.re), Cosh.cosh(c.im)),
                                            Multiplication(Cos.cos(c.re), Sinh.sinh(c.im)))
    }
}
