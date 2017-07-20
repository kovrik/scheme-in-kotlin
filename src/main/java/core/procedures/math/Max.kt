package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Max : AFn<Any?, Number?>(name = "max", isPure = true, minArgs = 1,
                mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java), restArgsType = Type.Real::class.java) {

    override operator fun invoke(args: Array<out Any?>): Number? {
        if (args.size == 1) {
            return args[0] as Number?
        }
        var result = args[0]!! as Number
        args.forEach { result = max(result, it!! as Number) }
        return result
    }

    private fun max(f: Number, s: Number) = when {
        f is Int        && s is Int        -> maxOf(f, s)
        f is Long       && s is Long       -> maxOf(f, s)
        f is Float      && s is Float      -> maxOf(f, s)
        f is Double     && s is Double     -> maxOf(f, s)
        f is BigRatio   && s is BigRatio   -> maxOf(f, s)
        f is BigInteger && s is BigInteger -> maxOf(f, s)
        f is BigDecimal && s is BigDecimal -> maxOf(f, s)
        f is BigDecimal                    -> maxOf(f, Utils.toBigDecimal(s))
        s is BigDecimal                    -> maxOf(s, Utils.toBigDecimal(f))
        f.toDouble() >= s.toDouble()       -> f
        else                               -> s
    }
}
