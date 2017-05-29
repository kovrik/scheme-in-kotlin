package core.procedures.characters

import core.procedures.AFn
import core.procedures.FnArgs

import java.util.function.BiPredicate

class CharComparison private constructor(override val name: String, private val predicate: BiPredicate<Char?, Char?>) :
        AFn(FnArgs(min = 2, mandatory = arrayOf<Class<*>>(Char::class.javaObjectType, Char::class.javaObjectType), rest = Char::class.javaObjectType)) {

    companion object {
        val CHAR_EQ          = CharComparison("char=?",     BiPredicate<Char?, Char?>{ arg1, arg2 -> arg1!!.compareTo(arg2!!) == 0 })
        val CHAR_LE          = CharComparison("char<?",     BiPredicate<Char?, Char?>{ arg1, arg2 -> arg1!!.compareTo(arg2!!) <  0 })
        val CHAR_GR          = CharComparison("char>?",     BiPredicate<Char?, Char?>{ arg1, arg2 -> arg1!!.compareTo(arg2!!) >  0 })
        val CHAR_LE_OR_EQ    = CharComparison("char<=?",    BiPredicate<Char?, Char?>{ arg1, arg2 -> arg1!!.compareTo(arg2!!) <= 0 })
        val CHAR_GR_OR_EQ    = CharComparison("char>=?",    BiPredicate<Char?, Char?>{ arg1, arg2 -> arg1!!.compareTo(arg2!!) >= 0 })
        val CHAR_EQ_CI       = CharComparison("char-ci=?",  BiPredicate<Char?, Char?>{ arg1, arg2 -> Character.toLowerCase(arg1!!) == Character.toLowerCase(arg2!!) })
        val CHAR_LE_CI       = CharComparison("char-ci<?",  BiPredicate<Char?, Char?>{ arg1, arg2 -> Character.toLowerCase(arg1!!) <  Character.toLowerCase(arg2!!) })
        val CHAR_GR_CI       = CharComparison("char-ci>?",  BiPredicate<Char?, Char?>{ arg1, arg2 -> Character.toLowerCase(arg1!!) >  Character.toLowerCase(arg2!!) })
        val CHAR_LE_OR_EQ_CI = CharComparison("char-ci<=?", BiPredicate<Char?, Char?>{ arg1, arg2 -> Character.toLowerCase(arg1!!) <= Character.toLowerCase(arg2!!) })
        val CHAR_GR_OR_EQ_CI = CharComparison("char-ci>=?", BiPredicate<Char?, Char?>{ arg1, arg2 -> Character.toLowerCase(arg1!!) >= Character.toLowerCase(arg2!!) })
    }

    override val isPure = true

    override operator fun invoke(vararg args: Any?): Boolean? {
        for (i in 0..args.size - 1 - 1) {
            if (!predicate.test(args[i] as Char, args[i + 1] as Char)) {
                return false
            }
        }
        return true
    }
}
