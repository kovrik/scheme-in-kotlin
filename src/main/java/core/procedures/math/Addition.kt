package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex
import core.scm.BigRatio
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class Addition : AFn(FnArgs(rest = Number::class.java)) {

    override val isPure = true
    override val name = "+"

    override operator fun invoke(vararg args: Any?): Number? {
        when (args.size) {
            0 -> return 0L
            1 -> return args[0] as Number?
            else -> {
                var result: Any? = 0L
                for (arg in args) {
                    arg!!
                    result = add(result as Number, arg as Number)
                }
                return result as Number
            }
        }
    }

    companion object {

        fun add(first: Number?, second: Number?): Number? {
            var (f, s) = Utils.upcast(first!!, second!!)
            when {
                /* Special cases */
                Utils.isPositiveInfinity(f) && Utils.isNegativeInfinity(s) -> return Double.NaN
                Utils.isPositiveInfinity(s) && Utils.isNegativeInfinity(f) -> return Double.NaN
                !Utils.isFinite(f)                 -> return f
                !Utils.isFinite(s)                 -> return s
                Utils.isZero(f)                    -> return Utils.inexactnessTaint(s, f)
                Utils.isZero(s)                    -> return Utils.inexactnessTaint(f, s)
                f is BigComplex && s is BigComplex -> return f.plus(s)
                f is BigRatio   && s is BigRatio   -> return f.plus(s)
                f is BigDecimal && s is BigDecimal -> return f.add(s)
                f is BigInteger && s is BigInteger -> return f.add(s)
                f is Double     && s is Double     -> return f + s
                f is Float      && s is Float      -> return f + s
                else -> {
                    val f = f.toLong()
                    val s = s.toLong()
                    try {
                        return Math.addExact(f, s)
                    } catch (e: ArithmeticException) {
                        return BigDecimal.valueOf(f).add(BigDecimal.valueOf(s))
                    }
                }
            }
        }
    }
}
