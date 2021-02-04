package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.Ratio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger
import kotlin.math.min

class Min : AFn<Any?, Number?>(name = "min", isPure = true, arity = AtLeast(1),
                               mandatoryArgsTypes = arrayOf(Type.Real::class.java),
                               restArgsType = Type.Real::class.java) {

    override operator fun invoke(args: Array<out Any?>): Number? = when (args.size) {
        1    -> args[0] as Number?
        else -> args.fold(args[0] as Number?, this::invoke)
    }

    override operator fun invoke(f: Any?, s: Any?): Number = when {
        f is Int        && s is Int        -> min(f, s)
        f is Long       && s is Long       -> min(f, s)
        f is Float      && s is Float      -> min(f, s)
        f is Double     && s is Double     -> min(f, s)
        f is Ratio      && s is Ratio      -> minOf(f, s)
        f is BigInteger && s is BigInteger -> minOf(f, s)
        f is BigDecimal && s is BigDecimal -> minOf(f, s)
        f is BigDecimal && s is Number     -> minOf(f, Utils.toBigDecimal(s))
        s is BigDecimal                    -> minOf(s, Utils.toBigDecimal(f!!))
        else                               -> if ((f!! as Number).toDouble() <= (s!! as Number).toDouble()) f as Number else s as Number
    }
}
