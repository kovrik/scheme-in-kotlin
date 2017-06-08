package core.procedures.interop

import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.Utils

import java.math.BigInteger

class BigIntegerType : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "bigint"

    override operator fun invoke(arg: Any?) = when (arg) {
        is Number -> Utils.toBigInteger(arg)
        else      -> BigInteger(arg!!.toString())
    }
}
