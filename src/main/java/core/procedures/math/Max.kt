package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Max : AFn(FnArgs(min = 1, mandatory = arrayOf<Class<*>>(Type.Real::class.java), rest = Type.Real::class.java)) {

    override val isPure = true
    override val name = "max"

    override operator fun invoke(vararg args: Any?): Number? {
        if (args.size == 1) {
            return args[0] as Number
        }
        var result = args[0]
        for (arg in args) {
            result = max(result as Number, arg as Number)
        }
        return result as Number
    }

    private fun max(first: Number?, second: Number?): Number {
        if (first  == null) throw NullPointerException()
        if (second == null) throw NullPointerException()
        var first = first
        var second = second
        /* Big Ratio numbers */
        if (first is BigRatio && second is BigRatio) {
            return if (first > second) first else second
        }
        if (first is BigRatio) {
            first = first.toDouble()
        }
        if (second is BigRatio) {
            second = second.toDouble()
        }
        if (first is Int && second is Int) {
            return Math.max(first, second)
        }
        if (first is Long && second is Long) {
            return Math.max(first, second)
        }
        if (first is Float && second is Float) {
            return Math.max(first, second)
        }
        if (first is Double && second is Double) {
            return Math.max(first, second)
        }
        if (first is BigInteger && second is BigInteger) {
            return first.max(second)
        }
        if (first is BigDecimal && second is BigDecimal) {
            return first.max(second)
        }
        if (first is BigDecimal) {
            val i = first.compareTo(Utils.toBigDecimal(second))
            return if (i < 0) second else first
        }
        if (second is BigDecimal) {
            val i = second.compareTo(Utils.toBigDecimal(first))
            return if (i < 0) first else second
        }
        if (first.toDouble() == second.toDouble()) {
            return first
        } else if (first.toDouble() > second.toDouble()) {
            return first
        }
        return second
    }
}
