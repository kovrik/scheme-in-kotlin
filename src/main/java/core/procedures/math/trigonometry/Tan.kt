package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.BigComplex
import core.utils.Utils

import kotlin.math.tan

class Tan : AFn<Number?, Number>(name = "tan", isPure = true, minArgs = 1, maxArgs = 1,
                                 mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = when {
        Utils.isZero(arg) -> 0L
        arg is BigComplex -> tan(arg)
        else              -> tan(arg!!.toDouble())
    }

    private fun tan(c: BigComplex): BigComplex = Sin.sin(c) / Cos.cos(c)
}
