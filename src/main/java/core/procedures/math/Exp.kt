package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import kotlin.math.exp

class Exp : AFn<Number?, Number>(name = "exp", isPure = true, minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    private val E = BigDecimal("2.71828182845904523536028747135266249775724709369995")

    private val expt = Expt()

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> Utils.inexactnessTaint(1L, arg)
        arg is Double || arg is Float -> when {
            Utils.isNegativeInfinity(arg) -> 0L
            !Utils.isFinite(arg)          -> arg
            else                          -> exp(arg.toDouble())
        }
        arg is Long || arg is Int || arg is Short || arg is Byte -> exp(arg.toDouble())
        arg is BigRatio && arg.isOne -> exp(1.0)
        else -> expt(E, arg!!)
    }
}
