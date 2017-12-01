package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.utils.Utils

class Tanh : AFn<Number?, Number>(name = "tanh", isPure = true, minArgs = 1, maxArgs = 1,
                                  mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is BigComplex -> tanh(arg)
        else              -> kotlin.math.tanh(arg!!.toDouble())
    }

    private fun tanh(c: BigComplex): Number {
        val sinh = Sinh.sinh(c)
        if (!Utils.isFinite(sinh)) {
            return sinh
        }
        val cosh = Cosh.cosh(c)
        if (!Utils.isFinite(cosh)) {
            return cosh
        }
        return (sinh as BigComplex) / cosh
    }
}
