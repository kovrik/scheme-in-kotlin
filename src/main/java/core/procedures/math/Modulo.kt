package core.procedures.math

import core.procedures.AFn
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

open class Modulo : AFn<Any?, Number?>(name = "modulo", isPure = true, minArgs = 2, maxArgs = 2,
                        mandatoryArgsTypes = arrayOf<Class<*>>(Long::class.javaObjectType, Long::class.javaObjectType)) {

    private val REM = Remainder()

    override operator fun invoke(arg1: Any?, arg2: Any?): Number? = when {
        Utils.isZero(arg2!!) -> throw ArithmeticException("modulo: undefined for 0")
        else -> invoke(arg1!! as Number, arg2 as Number)
    }

    private operator fun invoke(first: BigDecimal, second: BigDecimal) = first.remainder(second).let {
        when {
            it.signum() == 0 -> it
            first.signum() > 0 == second.signum() > 0 -> it
            else -> second.add(it)
        }
    }

    private operator fun invoke(first: BigInteger, second: BigInteger) = first.remainder(second).let {
        when {
            it.signum() == 0 -> it
            first.signum() > 0 == second.signum() > 0 -> it
            else -> second.add(it)
        }
    }

    private operator fun invoke(first: Number, second: Number): Number? {
        val (f, s) = Utils.upcast(first, second)
        when {
            Utils.isZero(f)                    -> return Utils.inexactnessTaint(f, s)
            f is BigDecimal && s is BigDecimal -> return invoke(f, s)
            f is BigInteger && s is BigInteger -> return invoke(f, s)
        }
        val m = REM(f, s)
        return when {
            m.toInt() == 0                   -> m
            f.toLong() > 0 == s.toLong() > 0 -> m
            f is Double && s is Double       -> m.toDouble() + s.toDouble()
            f is Float  && s is Float        -> m.toDouble() + s.toDouble()
            else                             -> m.toLong() + s.toLong()
        }
    }
}
