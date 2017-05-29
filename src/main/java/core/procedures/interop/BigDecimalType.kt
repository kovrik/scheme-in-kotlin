package core.procedures.interop

import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.Utils

import java.math.BigDecimal

class BigDecimalType : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "bigdec"

    override operator fun invoke(arg: Any?): BigDecimal {
        if (arg is Number) {
            return Utils.toBigDecimal((arg as Number?)!!)
        }
        return BigDecimal(arg!!.toString())
    }
}
