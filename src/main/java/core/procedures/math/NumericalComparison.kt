package core.procedures.math

import core.procedures.AFn
import core.scm.Type
import core.utils.Utils

class NumericalComparison private constructor(override val name: String, private val predicate: (Comparable<Number>, Number) -> Boolean) :
        AFn(isPure = true, minArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java, Type.Real::class.java),
            restArgsType = Type.Real::class.java) {

    companion object {
        val EQUAL         = NumericalComparison("=",  { f, s -> f == s } )
        val LESS          = NumericalComparison("<",  { f, s -> f <  s } )
        val GREATER       = NumericalComparison(">",  { f, s -> f >  s } )
        val LESS_EQUAL    = NumericalComparison("<=", { f, s -> f <= s } )
        val GREATER_EQUAL = NumericalComparison(">=", { f, s -> f >= s } )
    }

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        val (f, s) = Utils.upcast(arg1!! as Number, arg2!! as Number)
        return predicate(f as Comparable<Number>, s)
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
