package core.procedures.interop

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

import java.math.BigDecimal

class BigDecimalType : AFn<Any?, BigDecimal>(name = "bigdec", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Utils.toBigDecimal(arg)
}
