package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex
import core.utils.Utils

class Tanh : AFn<Number?, Number>(name = "tanh", isPure = true, arity = Exactly(1),
                                  mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is Complex -> tanh(arg)
        else              -> kotlin.math.tanh(arg!!.toDouble())
    }

    private fun tanh(c: Complex): Number {
        val sinh = Sinh.sinh(c)
        if (!Utils.isFinite(sinh)) {
            return sinh
        }
        val cosh = Cosh.cosh(c)
        if (!Utils.isFinite(cosh)) {
            return cosh
        }
        return (sinh as Complex) / cosh
    }
}
