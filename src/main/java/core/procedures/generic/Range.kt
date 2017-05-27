package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.math.Addition
import core.procedures.math.NumericalComparison
import core.scm.BigRatio
import core.scm.Cons
import core.scm.Type
import core.utils.Utils
import java.math.BigDecimal

class Range : AFn(FnArgsBuilder().min(0).max(3).rest(Type.Real::class.java).build()) {

    override val isPure = true
    override val name = "range"

    // TODO Write Unit tests!!!
    override operator fun invoke(vararg args: Any?): List<Any?>? {
        if (args.isEmpty()) {
            return Cons.EMPTY
        }
        var fraction = args[0] is BigRatio
        if (args.size == 3) {
            fraction = fraction || args[2] is BigRatio
        }
        var big = args[0] is BigDecimal
        if (args.size == 2) {
            big = big || args[1] is BigDecimal
        }
        if (args.size == 3) {
            big = big || args[1] is BigDecimal || args[2] is BigDecimal
        }
        if (fraction || big) {
            return range(args)
        }

        var exact = Utils.isExactInteger(args[0])
        if (args.size == 3) {
            exact = exact && Utils.isExactInteger(args[2])
        }
        val result = Cons.list<Number>()
        if (exact) {
            var start = 0L
            var end   = 0L
            var step  = 1L
            if (args.size == 1) {
                end = if (args[0] is Double) Math.ceil(args[0] as Double).toLong() else (args[0] as Number).toLong()
            } else if (args.size == 2) {
                start = (args[0] as Number).toLong()
                end   = if (args[1] is Double) Math.ceil(args[1] as Double).toLong() else (args[1] as Number).toLong()
            } else if (args.size == 3) {
                start = (args[0] as Number).toLong()
                step  = (args[2] as Number).toLong()
                if (step > 0) {
                    end = if (args[1] is Double) Math.ceil(args[1] as Double).toLong() else (args[1] as Number).toLong()
                } else {
                    end = if (args[1] is Double) Math.floor(args[1] as Double).toLong() else (args[1] as Number).toLong()
                }
            }
            if (step >= 0) {
                var n = start
                while (n < end) {
                    result.add(n)
                    n += step
                }
            } else {
                var n = start
                while (n > end) {
                    result.add(n)
                    n += step
                }
            }
        } else {
            var start = 0.0
            var end   = 0.0
            var step  = 1.0
            if (args.size == 1) {
                end   = (args[0] as Number).toDouble()
            } else if (args.size == 2) {
                start = (args[0] as Number).toDouble()
                end   = (args[1] as Number).toDouble()
            } else if (args.size == 3) {
                start = (args[0] as Number).toDouble()
                end   = (args[1] as Number).toDouble()
                step  = (args[2] as Number).toDouble()
            }
            if (step >= 0) {
                var n = start
                while (n < end) {
                    result.add(n)
                    n += step
                }
            } else {
                var n = start
                while (n > end) {
                    result.add(n)
                    n += step
                }
            }
        }
        return result
    }

    private fun range(vararg args: Any?): List<Any?> {
        val result = Cons.list<Number>()
        var start: Number = 0L
        var end:   Number = 0L
        var step:  Number = 1L
        if (args.size == 1) {
            end   = args[0] as Number
        } else if (args.size == 2) {
            start = args[0] as Number
            end   = args[1] as Number
        } else if (args.size == 3) {
            start = args[0] as Number
            end   = args[1] as Number
            step  = args[2] as Number
        }
        var cur: Number? = start
        var pred = NumericalComparison.LESS
        if (Utils.isNegative(step)) {
            pred = NumericalComparison.GREATER
        }
        while (pred(cur, end)) {
            result.add(cur!!)
            cur = Addition.add(cur, step)
        }
        return result
    }
}
