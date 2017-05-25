package core.procedures.characters

import core.procedures.AFn
import core.procedures.FnArgsBuilder

import java.util.function.Predicate

class CharPredicate private constructor(override val name: String, private val predicate: Predicate<Char>) : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Char::class.javaObjectType)).build()) {

    companion object {
        val IS_CHAR_WHITESPACE = CharPredicate("char-whitespace?", Predicate<Char> { Character.isWhitespace(it) })
        val IS_CHAR_ALPHABETIC = CharPredicate("char-alphabetic?", Predicate<Char> { Character.isAlphabetic(it.toInt()) })
        val IS_CHAR_UPPER_CASE = CharPredicate("char-upper-case?", Predicate<Char> { Character.isUpperCase(it) })
        val IS_CHAR_LOWER_CASE = CharPredicate("char-lower-case?", Predicate<Char> { Character.isLowerCase(it) })
        val IS_CHAR_NUMERIC    = CharPredicate("char-numeric?",    Predicate<Char> { Character.isDigit(it) })
    }

    override operator fun invoke(arg: Any?): Boolean {
        return predicate.test(arg as Char)
    }
}
