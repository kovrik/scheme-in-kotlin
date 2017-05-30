package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.scm.Type
import java.lang.NullPointerException

import java.math.BigDecimal
import java.math.BigInteger

class Abs : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.Real::class.java))) {

    override val isPure = true
    override val name = "abs"

    override operator fun invoke(arg: Any?): Number? {
        if (arg == null) throw NullPointerException()
        return abs(arg as Number)
    }

    companion object {
        fun abs(number: Number): Number {
            return when (number) {
                is Long       -> Math.abs(number)
                is Int        -> Math.abs(number)
                is Double     -> Math.abs(number)
                is Float      -> Math.abs(number)
                is BigInteger -> number.abs()
                is BigDecimal -> number.abs()
                is BigRatio   -> number.abs()
                else          -> Math.abs(number.toLong())
            }
        }
    }
}
