package core.procedures.strings

import core.procedures.AFn

class StringComparison private constructor(override val name: String,
                                           inline private val predicate: (String, String) -> Boolean) :
        AFn<Any?, Boolean>(isPure = true, restArgsType = CharSequence::class.java) {

    companion object {
        val STRING_EQ          = StringComparison("string=?",     { f, s -> f == s })
        val STRING_EQ_CI       = StringComparison("string-ci=?",  { f, s -> f.equals(s, true) })
        val STRING_LE          = StringComparison("string<?",     { f, s -> f <  s })
        val STRING_GR          = StringComparison("string>?",     { f, s -> f >  s })
        val STRING_LE_OR_EQ    = StringComparison("string<=?",    { f, s -> f <= s })
        val STRING_GR_OR_EQ    = StringComparison("string>=?",    { f, s -> f >= s })
        val STRING_LE_CI       = StringComparison("string-ci<?",  { f, s -> f.toLowerCase() <  s.toLowerCase() })
        val STRING_GR_CI       = StringComparison("string-ci>?",  { f, s -> f.toLowerCase() >  s.toLowerCase() })
        val STRING_LE_OR_EQ_CI = StringComparison("string-ci<=?", { f, s -> f.toLowerCase() <= s.toLowerCase() })
        val STRING_GR_OR_EQ_CI = StringComparison("string-ci>=?", { f, s -> f.toLowerCase() >= s.toLowerCase() })
    }

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size < 2  -> true
        args.size == 2 -> predicate(args[0].toString(), args[1].toString())
        else           -> (0..args.size - 2).all { predicate(args[it].toString(), args[it + 1].toString()) }
    }
}
