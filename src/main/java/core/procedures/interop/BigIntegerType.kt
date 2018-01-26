package core.procedures.interop

import core.procedures.AFn
import core.utils.Utils

import java.math.BigInteger

class BigIntegerType : AFn<Any?, BigInteger>(name = "bigint", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = Utils.toBigInteger(arg)
}
