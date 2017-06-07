package core.procedures.characters

import core.procedures.AFn
import core.procedures.FnArgs

class CharPredicate private constructor(override val name: String, private val predicate: (Char) -> Boolean) :
        AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Char::class.javaObjectType))) {

    companion object {
        val IS_CHAR_WHITESPACE = CharPredicate("char-whitespace?", Character::isWhitespace)
        val IS_CHAR_ALPHABETIC = CharPredicate("char-alphabetic?", { Character.isAlphabetic(it.toInt()) })
        val IS_CHAR_UPPER_CASE = CharPredicate("char-upper-case?", Character::isUpperCase)
        val IS_CHAR_LOWER_CASE = CharPredicate("char-lower-case?", Character::isLowerCase)
        val IS_CHAR_NUMERIC    = CharPredicate("char-numeric?",    Character::isDigit)
    }

    override operator fun invoke(arg: Any?) = predicate(arg as Char)
}
