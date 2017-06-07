package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type
import core.utils.Utils

class NumericalComparison private constructor(override val name: String, private val type: ComparisonType) :
        AFn(FnArgs(min = 2, mandatory = arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java), rest = Type.Real::class.java)) {

    enum class ComparisonType {
        EQUAL, LESS, GREATER, LESS_EQUAL, GREATER_EQUAL
    }

    companion object {
        val EQUAL         = NumericalComparison("=",  ComparisonType.EQUAL)
        val LESS          = NumericalComparison("<",  ComparisonType.LESS)
        val GREATER       = NumericalComparison(">",  ComparisonType.GREATER)
        val LESS_EQUAL    = NumericalComparison("<=", ComparisonType.LESS_EQUAL)
        val GREATER_EQUAL = NumericalComparison(">=", ComparisonType.GREATER_EQUAL)
    }

    override val isPure = true

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        val (f, s) = Utils.upcast(arg1!! as Number, arg2!! as Number)
        return when (type) {
            ComparisonType.EQUAL         -> f as Comparable<Number> == s
            ComparisonType.LESS          -> f as Comparable<Number> <  s
            ComparisonType.GREATER       -> f as Comparable<Number> >  s
            ComparisonType.LESS_EQUAL    -> f as Comparable<Number> <= s
            ComparisonType.GREATER_EQUAL -> f as Comparable<Number> >= s
        }
    }

    override operator fun invoke(vararg args: Any?): Boolean {
        for (i in 0..args.size - 2) {
            if (!invoke(args[i], args[i + 1])) {
                return false
            }
        }
        return true
    }
}
