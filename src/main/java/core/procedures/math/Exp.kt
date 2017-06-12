package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal

class Exp : AFn<Number?, Number>(name = "exp", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = exp(arg)

    companion object {
        val E = BigDecimal("2.71828182845904523536028747135266249775724709369995")

        fun exp(number: Number?): Number {
            number!!
            return when {
                Utils.isZero(number) -> Utils.inexactnessTaint(1L, number)
                number is Double || number is Float -> {
                    when {
                        Utils.isNegativeInfinity(number) -> 0L
                        !Utils.isFinite(number)          -> number
                        else                             -> Math.exp(number.toDouble())
                    }
                }
                number is Long || number is Int || number is Short || number is Byte -> Math.exp(number.toDouble())
                number is BigRatio && number.isOne -> Math.exp(1.0)
                else -> Expt.expt(E, number)
            }
        }
    }
}
