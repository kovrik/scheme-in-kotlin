package core.procedures.interop

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils

import java.math.BigInteger

class BigIntegerType : AFn<Any?, BigInteger>(name = "bigint", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = Utils.toBigInteger(arg)
}
