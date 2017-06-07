package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Min : AFn(FnArgs(min = 1, mandatory = arrayOf<Class<*>>(Type.Real::class.java), rest = Type.Real::class.java)) {

    override val isPure = true
    override val name = "min"

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.size == 1) {
            return args[0] as Number
        }
        var result = args[0]!!
        for (arg in args) {
            result = min(result as Number, arg!! as Number)
        }
        return result as Number
    }

    private fun min(f: Number, s: Number) = when {
        f is Int        && s is Int        -> Math.min(f, s)
        f is Long       && s is Long       -> Math.min(f, s)
        f is Float      && s is Float      -> Math.min(f, s)
        f is Double     && s is Double     -> Math.min(f, s)
        f is BigRatio   && s is BigRatio   -> f.min(s)
        f is BigInteger && s is BigInteger -> f.min(s)
        f is BigDecimal && s is BigDecimal -> f.min(s)
        f is BigDecimal                    -> f.min(Utils.toBigDecimal(s))
        s is BigDecimal                    -> s.min(Utils.toBigDecimal(f))
        f.toDouble() <= s.toDouble()       -> f
        else                               -> s
    }
}
