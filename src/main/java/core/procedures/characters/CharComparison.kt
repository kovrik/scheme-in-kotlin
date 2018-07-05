package core.procedures.characters

import core.procedures.AFn
import core.procedures.Arity.AtLeast

class CharComparison private constructor(override val name: String,
                                         private inline val predicate: (Char, Char) -> Boolean) :
        AFn<Any?, Boolean>(arity = AtLeast(2), isPure = true,
                           mandatoryArgsTypes = arrayOf(Char::class.javaObjectType, Char::class.javaObjectType),
                           restArgsType = Char::class.javaObjectType) {

    companion object {
        val CHAR_EQ          = CharComparison("char=?",     { f, s -> f == s })
        val CHAR_LE          = CharComparison("char<?",     { f, s -> f <  s })
        val CHAR_GR          = CharComparison("char>?",     { f, s -> f >  s })
        val CHAR_LE_OR_EQ    = CharComparison("char<=?",    { f, s -> f <= s })
        val CHAR_GR_OR_EQ    = CharComparison("char>=?",    { f, s -> f >= s })
        val CHAR_EQ_CI       = CharComparison("char-ci=?",  { f, s -> f.toLowerCase() == s.toLowerCase() })
        val CHAR_LE_CI       = CharComparison("char-ci<?",  { f, s -> f.toLowerCase() <  s.toLowerCase() })
        val CHAR_GR_CI       = CharComparison("char-ci>?",  { f, s -> f.toLowerCase() >  s.toLowerCase() })
        val CHAR_LE_OR_EQ_CI = CharComparison("char-ci<=?", { f, s -> f.toLowerCase() <= s.toLowerCase() })
        val CHAR_GR_OR_EQ_CI = CharComparison("char-ci>=?", { f, s -> f.toLowerCase() >= s.toLowerCase() })
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = predicate(arg1!! as Char, arg2!! as Char)

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size < 2  -> true
        args.size == 2 -> invoke(args[0]!!, args[1]!!)
        else           -> (0..args.size - 2).all { invoke(args[it]!!, args[it + 1]!!) }
    }
}
