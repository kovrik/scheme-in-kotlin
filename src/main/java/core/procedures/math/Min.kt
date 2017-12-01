package core.procedures.math

import core.procedures.AFn
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Min : AFn<Any?, Number?>(name = "min", isPure = true, minArgs = 1,
                               mandatoryArgsTypes = arrayOf(Type.Real::class.java),
                               restArgsType = Type.Real::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> args[0] as Number?
        else -> args.fold(args[0] as Number?, this::min)
    }

    private fun min(f: Number?, s: Any?): Number? = when {
        f is Int        && s is Int        -> kotlin.math.min(f, s)
        f is Long       && s is Long       -> kotlin.math.min(f, s)
        f is Float      && s is Float      -> kotlin.math.min(f, s)
        f is Double     && s is Double     -> kotlin.math.min(f, s)
        f is BigRatio   && s is BigRatio   -> minOf(f, s)
        f is BigInteger && s is BigInteger -> minOf(f, s)
        f is BigDecimal && s is BigDecimal -> minOf(f, s)
        f is BigDecimal && s is Number     -> minOf(f, Utils.toBigDecimal(s))
        s is BigDecimal                    -> minOf(s, Utils.toBigDecimal(f!!))
        else                               -> if (f!!.toDouble() <= (s!! as Number).toDouble()) f else s as Number
    }
}
