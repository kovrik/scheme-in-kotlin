package core.procedures.strings

import core.procedures.AFn

class StringComparison private constructor(override val name: String, private val predicate: (String, String) -> Boolean ) :
        AFn<Any?, Boolean>(isPure = true, restArgsType = CharSequence::class.java) {

    companion object {
        val STRING_EQ          = StringComparison("string=?",     { arg1, arg2 -> arg1 == arg2 })
        val STRING_EQ_CI       = StringComparison("string-ci=?",  { arg1, arg2 -> arg1.equals(arg2, true) })
        val STRING_LE          = StringComparison("string<?",     { arg1, arg2 -> arg1 <  arg2 })
        val STRING_GR          = StringComparison("string>?",     { arg1, arg2 -> arg1 >  arg2 })
        val STRING_LE_OR_EQ    = StringComparison("string<=?",    { arg1, arg2 -> arg1 <= arg2 })
        val STRING_GR_OR_EQ    = StringComparison("string>=?",    { arg1, arg2 -> arg1 >= arg2 })
        val STRING_LE_CI       = StringComparison("string-ci<?",  { arg1, arg2 -> arg1.toLowerCase() <  arg2.toLowerCase() })
        val STRING_GR_CI       = StringComparison("string-ci>?",  { arg1, arg2 -> arg1.toLowerCase() >  arg2.toLowerCase() })
        val STRING_LE_OR_EQ_CI = StringComparison("string-ci<=?", { arg1, arg2 -> arg1.toLowerCase() <= arg2.toLowerCase() })
        val STRING_GR_OR_EQ_CI = StringComparison("string-ci>=?", { arg1, arg2 -> arg1.toLowerCase() >= arg2.toLowerCase() })
    }

    override operator fun invoke(args: Array<out Any?>): Boolean {
        if (args.size < 2) return true
        return (0..args.size - 2).any { predicate(args[it].toString(), args[it + 1].toString()) }
    }
}
