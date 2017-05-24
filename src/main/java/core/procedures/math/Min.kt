package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class Min : AFn(FnArgsBuilder().min(1).mandatory(arrayOf<Class<*>>(Type.Real::class.java))
        .rest(Type.Real::class.java).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "min"

    override fun apply(args: Array<Any?>): Number? {
        if (args.size == 1) {
            return args[0] as Number
        }
        var result = args[0]
        for (arg in args) {
            result = min(result as Number, arg as Number)
        }
        return result as Number
    }

    private fun min(first: Number?, second: Number?): Number {
        if (first  == null) throw NullPointerException()
        if (second == null) throw NullPointerException()
        var first = first
        var second = second
        if (first is BigRatio && second is BigRatio) {
            return if (first.compareTo(second) < 0) first else second
        }
        if (first is BigRatio) {
            first = first.toDouble()
        }
        if (second is BigRatio) {
            second = second.toDouble()
        }
        if (first is Int && second is Int) {
            return Math.min(first, second)
        }
        if (first is Long && second is Long) {
            return Math.min(first, second)
        }
        if (first is Float && second is Float) {
            return Math.min(first, second)
        }
        if (first is Double && second is Double) {
            return Math.min(first, second)
        }
        if (first is BigInteger && second is BigInteger) {
            return first.min(second)
        }
        if (first is BigDecimal && second is BigDecimal) {
            return first.min(second)
        }
        if (first is BigDecimal) {
            val i = first.compareTo(Utils.toBigDecimal(second))
            return if (i > 0) second else first
        }
        if (second is BigDecimal) {
            val i = second.compareTo(Utils.toBigDecimal(first))
            return if (i > 0) first else second
        }
        if (first.toDouble() == second.toDouble()) {
            return first
        } else if (first.toDouble() < second.toDouble()) {
            return first
        }
        return second
    }
}