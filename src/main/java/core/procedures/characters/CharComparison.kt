package core.procedures.characters

import core.procedures.AFn

class CharComparison private constructor(override val name: String,
                                         inline private val predicate: (Char?, Char?) -> Boolean) :
        AFn<Any?, Boolean>(minArgs = 2,
                           mandatoryArgsTypes = arrayOf<Class<*>>(Char::class.javaObjectType, Char::class.javaObjectType),
                           restArgsType = Char::class.javaObjectType) {

    companion object {
        val CHAR_EQ          = CharComparison("char=?",     { arg1, arg2 -> arg1!! == arg2!! })
        val CHAR_LE          = CharComparison("char<?",     { arg1, arg2 -> arg1!! <  arg2!! })
        val CHAR_GR          = CharComparison("char>?",     { arg1, arg2 -> arg1!! >  arg2!! })
        val CHAR_LE_OR_EQ    = CharComparison("char<=?",    { arg1, arg2 -> arg1!! <= arg2!! })
        val CHAR_GR_OR_EQ    = CharComparison("char>=?",    { arg1, arg2 -> arg1!! >= arg2!! })
        val CHAR_EQ_CI       = CharComparison("char-ci=?",  { arg1, arg2 -> arg1!!.toLowerCase() == arg2!!.toLowerCase() })
        val CHAR_LE_CI       = CharComparison("char-ci<?",  { arg1, arg2 -> arg1!!.toLowerCase() <  arg2!!.toLowerCase() })
        val CHAR_GR_CI       = CharComparison("char-ci>?",  { arg1, arg2 -> arg1!!.toLowerCase() >  arg2!!.toLowerCase() })
        val CHAR_LE_OR_EQ_CI = CharComparison("char-ci<=?", { arg1, arg2 -> arg1!!.toLowerCase() <= arg2!!.toLowerCase() })
        val CHAR_GR_OR_EQ_CI = CharComparison("char-ci>=?", { arg1, arg2 -> arg1!!.toLowerCase() >= arg2!!.toLowerCase() })
    }

    override val isPure = true

    override operator fun invoke(args: Array<out Any?>): Boolean {
        for (i in 0..args.size - 2) {
            if (!predicate(args[i] as Char, args[i + 1] as Char)) {
                return false
            }
        }
        return true
    }
}
