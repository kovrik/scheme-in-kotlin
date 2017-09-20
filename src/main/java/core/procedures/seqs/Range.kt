package core.procedures.seqs

import core.procedures.AFn
import core.procedures.math.Addition
import core.procedures.math.NumericalComparison
import core.scm.BigRatio
import core.scm.Type
import core.utils.Utils
import java.math.BigDecimal
import java.math.BigInteger

class Range : AFn<Any?, Any?>(name = "range", isPure = true, maxArgs = 3, restArgsType = Type.Real::class.java) {

    private val addition = Addition()

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.isEmpty()) {
            return range()
        }
        var fraction = args[0] is BigRatio
        if (args.size == 3) {
            fraction = fraction || args[2] is BigRatio
        }
        val big = args.any { it is BigDecimal || it is BigInteger }
        if (fraction || big) {
            return when (args.size) {
                1    -> range(0L, args[0] as Number)
                2    -> range(args[0] as Number, args[1] as Number)
                else -> range(args[0] as Number, args[1] as Number, args[2] as Number)
            }
        }
        var exact = Utils.isExactInteger(args[0])
        if (args.size == 3) {
            exact = exact && Utils.isExactInteger(args[2])
        }
        return when {
            exact -> when (args.size) {
                1    -> rangeLong(0L, (args[0] as Number).toLong())
                2    -> rangeLong((args[0] as Number).toLong(), (args[1] as Number).toLong())
                else -> rangeLong((args[0] as Number).toLong(), (args[1] as Number).toLong(), (args[2] as Number).toLong())
            }
            else -> when (args.size) {
                1    -> rangeDouble(0.0, (args[0] as Number).toDouble())
                2    -> rangeDouble((args[0] as Number).toDouble(), (args[1] as Number).toDouble())
                else -> rangeDouble((args[0] as Number).toDouble(), (args[1] as Number).toDouble(), (args[2] as Number).toDouble())
            }
        }
    }

    private fun range() = object : Sequence<Number> {
        override fun iterator() = object : Iterator<Number> {

            private var big = false
            private var next: Number = 0L

            override fun hasNext() = true

            override fun next(): Number {
                val result = next
                next = if (big) {
                    addition.add(next, 1L)!!
                } else {
                    try {
                        Math.addExact(next.toLong(), 1L)
                    } catch (e: ArithmeticException) {
                        big = true
                        addition.add(next, 1L)!!
                    }
                }
                return result
            }
        }
    }

    private fun range(start: Number = 0L, end: Number = 1L, step: Number = 1L): Sequence<Number> {
        val pred = when (Utils.isNegative(step)) {
            true  -> NumericalComparison.GREATER
            false -> NumericalComparison.LESS
        }
        if (!pred(start, end)) {
            return emptySequence()
        }
        return generateSequence(start, {
            val result = addition.add(it, step)
            when {
                pred(result, end) -> result
                else -> null
            }
        })
    }

    private fun rangeLong(start: Long = 0L, end: Long = 1L, step: Long = 1L) = when (step > 0) {
        true -> when (start < end) {
            true -> generateSequence(start, {
                val result = it + step
                when (result < end) {
                    true  -> result
                    false -> null
                }
            })
            else -> emptySequence()
        }
        false -> when (start > end) {
            true -> generateSequence(start, {
                val result = it + step
                when (result > end) {
                    true  -> result
                    false -> null
                }
            })
            else -> emptySequence()
        }
    }

    private fun rangeDouble(start: Double = 0.0, end: Double = 1.0, step: Double = 1.0) = when (step > 0) {
        true -> when (start < end) {
            true -> generateSequence(start, {
                val result = it + step
                when (result < end) {
                    true  -> result
                    false -> null
                }
            })
            else -> emptySequence()
        }
        false -> when (start > end) {
            true -> generateSequence(start, {
                val result = it + step
                when (result > end) {
                    true  -> result
                    false -> null
                }
            })
            else -> emptySequence()
        }
    }
}
