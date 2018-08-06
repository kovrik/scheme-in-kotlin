package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Complex
import core.utils.Utils

import kotlin.math.tan

class Tan : AFn<Number?, Number>(name = "tan", isPure = true, arity = Exactly(1),
                                 mandatoryArgsTypes = arrayOf(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is Complex -> tan(arg)
        else              -> tan(arg!!.toDouble())
    }

    private fun tan(c: Complex): Complex = Sin.sin(c) / Cos.cos(c)
}
