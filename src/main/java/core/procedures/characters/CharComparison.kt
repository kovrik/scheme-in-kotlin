package core.procedures.characters

import core.procedures.AFn
import core.procedures.FnArgsBuilder

import java.util.function.BiPredicate

class CharComparison private constructor(override val name: String, private val predicate: BiPredicate<Char?, Char?>) : AFn(FnArgsBuilder().min(2)
        .mandatory(arrayOf<Class<*>>(Char::class.javaObjectType, Char::class.javaObjectType))
        .rest(Char::class.javaObjectType).build()) {

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

    override val isPure: Boolean
        get() = true

    override fun apply(args: Array<Any?>): Boolean? {
        for (i in 0..args.size - 1 - 1) {
            if (!predicate.test(args[i] as Char, args[i + 1] as Char)) {
                return java.lang.Boolean.FALSE
            }
        }
        return java.lang.Boolean.TRUE
    }
}
