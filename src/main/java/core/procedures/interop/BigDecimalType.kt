package core.procedures.interop

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils

import java.math.BigDecimal

class BigDecimalType : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "bigdec"

    override operator fun invoke(arg: Any?): BigDecimal {
        if (arg is Number) {
            return Utils.toBigDecimal((arg as Number?)!!)
        }
        return BigDecimal(arg!!.toString())
    }
}
