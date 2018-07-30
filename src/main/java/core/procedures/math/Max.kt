package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger
import kotlin.math.max

class Max : AFn<Any?, Number?>(name = "max", isPure = true, arity = AtLeast(1),
                               mandatoryArgsTypes = arrayOf(Type.Real::class.java),
                               restArgsType = Type.Real::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> args[0] as Number?
        else -> args.fold(args[0] as Number?, this::invoke)
    }

    override operator fun invoke(f: Any?, s: Any?): Number? = when {
        f is Int        && s is Int        -> max(f, s)
        f is Long       && s is Long       -> max(f, s)
        f is Float      && s is Float      -> max(f, s)
        f is Double     && s is Double     -> max(f, s)
        f is BigRatio   && s is BigRatio   -> maxOf(f, s)
        f is BigInteger && s is BigInteger -> maxOf(f, s)
        f is BigDecimal && s is BigDecimal -> maxOf(f, s)
        f is BigDecimal && s is Number     -> maxOf(f, Utils.toBigDecimal(s))
        s is BigDecimal                    -> maxOf(s, Utils.toBigDecimal(f!!))
        else                               -> if ((f!! as Number).toDouble() >= (s!! as Number).toDouble()) f as Number else s as Number
    }
}
