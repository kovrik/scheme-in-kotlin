package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

import java.util.function.BiPredicate

class StringComparison private constructor(override val name: String, private val predicate: BiPredicate<String, String>) :
        AFn(FnArgs(rest = CharSequence::class.java)) {

    companion object {
        val STRING_EQ          = StringComparison("string=?",     BiPredicate<String, String> { obj, anObject -> obj == anObject })
        val STRING_EQ_CI       = StringComparison("string-ci=?",  BiPredicate<String, String> { obj, anotherString -> obj.equals(anotherString, ignoreCase = true) })
        val STRING_LE          = StringComparison("string<?",     BiPredicate<String, String> { arg1, arg2 -> arg1 < arg2 })
        val STRING_GR          = StringComparison("string>?",     BiPredicate<String, String> { arg1, arg2 -> arg1 > arg2 })
        val STRING_LE_OR_EQ    = StringComparison("string<=?",    BiPredicate<String, String> { arg1, arg2 -> arg1 <= arg2 })
        val STRING_GR_OR_EQ    = StringComparison("string>=?",    BiPredicate<String, String> { arg1, arg2 -> arg1 >= arg2 })
        val STRING_LE_CI       = StringComparison("string-ci<?",  BiPredicate<String, String> { arg1, arg2 -> arg1.toLowerCase() < arg2.toLowerCase() })
        val STRING_GR_CI       = StringComparison("string-ci>?",  BiPredicate<String, String> { arg1, arg2 -> arg1.toLowerCase() > arg2.toLowerCase() })
        val STRING_LE_OR_EQ_CI = StringComparison("string-ci<=?", BiPredicate<String, String> { arg1, arg2 -> arg1.toLowerCase() <= arg2.toLowerCase() })
        val STRING_GR_OR_EQ_CI = StringComparison("string-ci>=?", BiPredicate<String, String> { arg1, arg2 -> arg1.toLowerCase() >= arg2.toLowerCase() })
    }

    override val isPure = true

    override operator fun invoke(vararg args: Any?): Boolean? {
        if (args.size < 2) return true
        return (0..args.size - 2).any { predicate.test(args[it].toString(), args[it + 1].toString()) }
    }
}
