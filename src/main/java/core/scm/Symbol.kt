package core.scm

import core.exceptions.ArityException
import core.procedures.AFn
import core.utils.InternPool

/* Symbol class
 *
 * By default all symbols are interned and stored in INTERNED Map.
 * This means that two values:
 *
 *   (define s1 'test) and (define s2 'test)
 *
 * will reference to the same symbol object.
 */
class Symbol (override val name: String, private val meta: Map<*, *>? = null) : AFn<Any?, Any?>(isPure = true), INamed, IMeta {

    companion object {
        /* Pool of all interned symbols */
        private val POOL = InternPool<Symbol>()

        fun intern(name: String?) = POOL.intern(Symbol(name!!))!!

        private val SPECIAL_CHARS = "()[]{}\",'`;|\\"
        /* Check if Symbol has Special Characters and needs to be escaped */
        private fun hasSpecialChars(name: String) = when {
            name.isEmpty() || Character.isDigit(name[0]) -> true
            name[0] == '#' && (name.length == 1 || name[1] != '%') -> true
            else -> name.toCharArray().any { Character.isWhitespace(it) || SPECIAL_CHARS.indexOf(it) > -1 }
        }
    }

    val isEscape: Boolean = hasSpecialChars(name)

    override fun meta() = meta

    override operator fun invoke(vararg args: Any?) = when {
        args.isEmpty() || args.size > 2 -> throw ArityException(toString() + " Symbol", 1, 2, args.size)
        else -> (args[0] as Map<Any?, Any?>).getOrDefault(this, args.getOrNull(1))
    }

    override fun equals(other: Any?) = when {
        this === other -> true
        other == null || other.javaClass != Symbol::class.java -> false
        else -> name == (other as Symbol).name
    }

    override fun hashCode() = name.hashCode() + 1037096266

    override fun toString() = name
}