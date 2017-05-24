package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger
import java.util.function.BiPredicate

class NumericalComparison private constructor(override val name: String, private val predicate: BiPredicate<Comparable<Number>, Number>) : AFn(FnArgsBuilder().min(2)
        .mandatory(arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java))
        .rest(Type.Real::class.java).build()) {

    companion object {
        val EQUAL         = NumericalComparison("=",  BiPredicate { f, s -> f.compareTo(s) == 0 })
        val LESS          = NumericalComparison("<",  BiPredicate { f, s -> f < s })
        val GREATER       = NumericalComparison(">",  BiPredicate { f, s -> f > s })
        val LESS_EQUAL    = NumericalComparison("<=", BiPredicate { f, s -> f <= s })
        val GREATER_EQUAL = NumericalComparison(">=", BiPredicate { f, s -> f >= s })
    }

    override val isPure: Boolean
        get() = true

    override fun apply2(arg1: Any?, arg2: Any?): Boolean? {
        return apply(arg1, arg2)
    }

    override fun apply(vararg args: Any?): Boolean {
        for (i in 0..args.size - 1 - 1) {
            var f = args[i] as Number
            var s = args[i + 1] as Number
            if (f is Double || s is Double) {
                f = f.toDouble()
                s = s.toDouble()
            } else if (f is Float || s is Float) {
                f = f.toFloat()
                s = s.toFloat()
            } else if (f is BigRatio || s is BigRatio) {
                /* Coerce BigRatio to BigDecimal */
                f = Utils.toBigDecimal(f)
                s = Utils.toBigDecimal(s)
            } else if (f is BigDecimal || s is BigDecimal) {
                f = Utils.toBigDecimal(f)
                s = Utils.toBigDecimal(s)
            } else if (s is BigInteger || f is BigInteger) {
                f = Utils.toBigInteger(f)
                s = Utils.toBigInteger(s)
            } else {
                f = f.toLong()
                s = s.toLong()
            }
            if (!predicate.test(f as Comparable<Number>, s)) {
                return java.lang.Boolean.FALSE
            }
        }
        return java.lang.Boolean.TRUE
    }
}
