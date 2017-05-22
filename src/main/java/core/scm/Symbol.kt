package core.scm

import core.exceptions.ArityException
import core.procedures.AFn
import core.utils.InternPool

/* Symbol class
 *
 * By default all symbols are interned and stored in INTERNED Map.
 *
 * This means that two values:
 *
 *   (define s1 'test)
 *   (define s2 'test)
 *
 * will reference to the same symbol object.
 */
class Symbol (override val name: String, private val meta: Map<*, *>?) : AFn(), INamed, IMeta {

    companion object {
        /* Pool of all interned symbols */
        private val POOL = InternPool<Symbol>()

        private val SPECIAL_CHARS = "()[]{}\",'`;|\\"

        fun intern(name: String?): Symbol? {
            // always intern symbols
            return POOL.intern(Symbol(name!!))
        }

        /* Check if Symbol has Special Characters and needs to be escaped */
        private fun hasSpecialChars(name: String): Boolean {
            /* Check if string representation must be escaped */
            if (name.isEmpty() || Character.isDigit(name[0])) {
                return true
            }
            if (name[0] == '#') {
                if (name.length == 1) {
                    return true
                }
                if (name[1] != '%') {
                    return true
                }
            }
            for (c in name.toCharArray()) {
                if (Character.isWhitespace(c) || SPECIAL_CHARS.indexOf(c) > -1) {
                    return true
                }
            }
            return false
        }
    }

    val isEscape: Boolean

    private constructor(name: String) : this(name, null)

    init {
        this.isEscape = hasSpecialChars(name)
    }

    override fun meta(): Map<*, *>? {
        return meta
    }

    override val isPure: Boolean
        get() = true

    override fun apply(args: Array<Any?>): Any? {
        if (args.isEmpty() || args.size > 2) {
            throw ArityException(toString() + " Symbol", 1, 2, args.size)
        }
        val defaultValue = if (args.size == 2) args[1] else null
        return (args[0] as Map<Any?, Any?>).getOrDefault(this, defaultValue)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || other.javaClass != Symbol::class.java) return false
        val o = other as Symbol?
        return name == o!!.name
    }

    override fun hashCode(): Int {
        return name.hashCode() + 1037096266
    }

    override fun toString(): String {
        return name
    }
}