package core.procedures.interop

import core.procedures.AFn
import core.utils.Utils

import java.math.BigDecimal

class BigDecimalType : AFn(name = "bigdec", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Number -> Utils.toBigDecimal(arg)
        else      -> BigDecimal(arg!!.toString())
    }
}
