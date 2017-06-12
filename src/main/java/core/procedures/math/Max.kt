package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Max : AFn<Any?, Number?>(name = "max", isPure = true, minArgs = 1,
                mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java), restArgsType = Type.Real::class.java) {

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.size == 1) {
            return args[0] as Number
        }
        var result = args[0]!! as Number
        for (arg in args) {
            result = max(result, arg!! as Number)
        }
        return result
    }

    private fun max(f: Number, s: Number) = when {
        f is Int        && s is Int        -> Math.max(f, s)
        f is Long       && s is Long       -> Math.max(f, s)
        f is Float      && s is Float      -> Math.max(f, s)
        f is Double     && s is Double     -> Math.max(f, s)
        f is BigRatio   && s is BigRatio   -> f.max(s)
        f is BigInteger && s is BigInteger -> f.max(s)
        f is BigDecimal && s is BigDecimal -> f.max(s)
        f is BigDecimal                    -> f.max(Utils.toBigDecimal(s))
        s is BigDecimal                    -> s.max(Utils.toBigDecimal(f))
        f.toDouble() >= s.toDouble()       -> f
        else                               -> s
    }
}
