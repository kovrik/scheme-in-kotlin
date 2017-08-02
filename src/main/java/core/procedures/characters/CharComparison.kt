package core.procedures.characters

import core.procedures.AFn

class CharComparison private constructor(override val name: String,
                                         inline private val predicate: (Char?, Char?) -> Boolean) :
        AFn<Any?, Boolean>(minArgs = 2, isPure = true,
                           mandatoryArgsTypes = arrayOf<Class<*>>(Char::class.javaObjectType, Char::class.javaObjectType),
                           restArgsType = Char::class.javaObjectType) {

    companion object {
        val CHAR_EQ          = CharComparison("char=?",     { f, s -> f!! == s!! })
        val CHAR_LE          = CharComparison("char<?",     { f, s -> f!! <  s!! })
        val CHAR_GR          = CharComparison("char>?",     { f, s -> f!! >  s!! })
        val CHAR_LE_OR_EQ    = CharComparison("char<=?",    { f, s -> f!! <= s!! })
        val CHAR_GR_OR_EQ    = CharComparison("char>=?",    { f, s -> f!! >= s!! })
        val CHAR_EQ_CI       = CharComparison("char-ci=?",  { f, s -> f!!.toLowerCase() == s!!.toLowerCase() })
        val CHAR_LE_CI       = CharComparison("char-ci<?",  { f, s -> f!!.toLowerCase() <  s!!.toLowerCase() })
        val CHAR_GR_CI       = CharComparison("char-ci>?",  { f, s -> f!!.toLowerCase() >  s!!.toLowerCase() })
        val CHAR_LE_OR_EQ_CI = CharComparison("char-ci<=?", { f, s -> f!!.toLowerCase() <= s!!.toLowerCase() })
        val CHAR_GR_OR_EQ_CI = CharComparison("char-ci>=?", { f, s -> f!!.toLowerCase() >= s!!.toLowerCase() })
    }

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size < 2 -> true
        else -> (0..args.size - 2).none { !predicate(args[it] as Char, args[it + 1] as Char) }
    }
}
