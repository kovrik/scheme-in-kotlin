package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type
import core.utils.Utils

import java.util.function.BiPredicate

class NumericalComparison private constructor(override val name: String, private val predicate: BiPredicate<Comparable<Number>, Number>) :
        AFn(FnArgs(min = 2, mandatory = arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java), rest = Type.Real::class.java)) {

    companion object {
        val EQUAL         = NumericalComparison("=",  BiPredicate { f, s -> f.compareTo(s) == 0 })
        val LESS          = NumericalComparison("<",  BiPredicate { f, s -> f < s })
        val GREATER       = NumericalComparison(">",  BiPredicate { f, s -> f > s })
        val LESS_EQUAL    = NumericalComparison("<=", BiPredicate { f, s -> f <= s })
        val GREATER_EQUAL = NumericalComparison(">=", BiPredicate { f, s -> f >= s })
    }

    override val isPure = true

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        val (f, s) = Utils.upcast(arg1!! as Number, arg2!! as Number)
        return predicate.test(f as Comparable<Number>, s)
    }

    override operator fun invoke(vararg args: Any?): Boolean {
        for (i in 0..args.size - 2) {
            val result = invoke(args[i], args[i + 1])
            if (!result) {
                return false
            }
        }
        return true
    }
}
