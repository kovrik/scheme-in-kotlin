package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.Type
import core.utils.Utils

class NumericalComparison private constructor(override val name: String,
                                              private inline val predicate: (Comparable<Number>, Number) -> Boolean) :
        AFn<Any?, Boolean>(isPure = true, arity = AtLeast(2),
                           mandatoryArgsTypes = arrayOf(Type.Real::class.java, Type.Real::class.java),
                           restArgsType = Type.Real::class.java) {

    companion object {
        val EQUAL         = NumericalComparison("=") { f, s -> f == s }
        val LESS          = NumericalComparison("<") { f, s -> f <  s }
        val GREATER       = NumericalComparison(">") { f, s -> f >  s }
        val LESS_EQUAL    = NumericalComparison("<=") { f, s -> f <= s }
        val GREATER_EQUAL = NumericalComparison(">=") { f, s -> f >= s }
    }

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        val (f, s) = Utils.upcast(arg1!! as Number, arg2!! as Number)
        return predicate(f as Comparable<Number>, s)
    }

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size < 2  -> true
        args.size == 2 -> invoke(args[0], args[1])
        else           -> (0..args.size - 2).all { invoke(args[it], args[it + 1]) }
    }
}
