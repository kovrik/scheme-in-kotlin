package core.scm

import core.exceptions.ArityException
import core.procedures.AFn
import core.utils.InternPool

class Keyword private constructor(override val name: String) :
        AFn<Any?, Any?>(isPure = true, mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)), INamed {

    companion object {
        /* Pool of all interned keywords */
        private val POOL = InternPool<Keyword>()

        fun intern(value: String) = POOL.intern(Keyword(value))
    }

    override fun toString() = ':' + name

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.isEmpty() || args.size > 2) {
            throw ArityException(toString() + " Keyword", 1, 2, args.size)
        }
        return (args[0] as Map<Any?, Any?>).getOrDefault(this, args.getOrNull(1))
    }

    override fun equals(other: Any?) = when {
        this === other -> true
        other == null || javaClass != other.javaClass -> false
        else -> name == (other as Keyword).name
    }

    override fun hashCode() = name.hashCode() + 1077096266
}
