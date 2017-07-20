package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Min : AFn<Any?, Number?>(name = "min", isPure = true, minArgs = 1,
                mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java), restArgsType = Type.Real::class.java) {

    override operator fun invoke(args: Array<out Any?>): Number? {
        if (args.size == 1) {
            return args[0] as Number?
        }
        var result = args[0]!! as Number
        args.forEach { result = min(result, it!! as Number) }
        return result
    }

    private fun min(f: Number, s: Number) = when {
        f is Int        && s is Int        -> minOf(f, s)
        f is Long       && s is Long       -> minOf(f, s)
        f is Float      && s is Float      -> minOf(f, s)
        f is Double     && s is Double     -> minOf(f, s)
        f is BigRatio   && s is BigRatio   -> minOf(f, s)
        f is BigInteger && s is BigInteger -> minOf(f, s)
        f is BigDecimal && s is BigDecimal -> minOf(f, s)
        f is BigDecimal                    -> minOf(f, Utils.toBigDecimal(s))
        s is BigDecimal                    -> minOf(s, Utils.toBigDecimal(f))
        f.toDouble() <= s.toDouble()       -> f
        else                               -> s
    }
}
