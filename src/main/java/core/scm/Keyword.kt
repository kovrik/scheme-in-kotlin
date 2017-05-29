package core.scm

import core.exceptions.ArityException
import core.procedures.AFn
import core.procedures.FnArgs
import core.utils.InternPool

class Keyword private constructor(override val name: String) : AFn(FnArgs(mandatory = arrayOf<Class<*>>(Map::class.java))), INamed {

    companion object {
        /* Pool of all interned keywords */
        private val POOL = InternPool<Keyword>()

        fun intern(value: String): Keyword {
            // always intern keywords
            return POOL.intern(Keyword(value))!!
        }
    }

    override val isPure = true

    override fun toString(): String {
        return ':' + name
    }

    override operator fun invoke(vararg args: Any?): Any? {
        if (args.isEmpty() || args.size > 2) {
            throw ArityException(toString() + " Keyword", 1, 2, args.size)
        }
        val defaultValue = if (args.size == 2) args[1] else null
        return (args[0] as Map<Any?, Any?>).getOrDefault(this, defaultValue)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || javaClass != other.javaClass) return false
        val that = other as Keyword?
        return name == that!!.name
    }

    override fun hashCode(): Int {
        return name.hashCode() + 1077096266
    }

}
