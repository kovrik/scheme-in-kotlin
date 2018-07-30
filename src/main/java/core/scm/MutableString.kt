package core.scm

/**
 * Mutable Scheme String

 * Java String is immutable, StringBuilder is mutable.
 * But both classes are final and cannot be extended.
 * This MutableString class holds a mutable StringBuilder instance
 * and delegates it all string operations.
 */
class MutableString(val string: StringBuilder) : CharSequence by string, Appendable by string {

    constructor() : this(StringBuilder())

    constructor(string: String) : this(StringBuilder(string))

    constructor(length: Int) : this(StringBuilder(length))

    fun append(c: Any?) = apply { string.append(c) }

    operator fun set(index: Int, ch: Char) = when {
        index >= length -> throw IndexOutOfBoundsException("string-set!: value out of range: $index")
        else            -> string[index] = ch
    }

    fun clear() = string.setLength(0)

    override fun equals(other: Any?) = when {
        this === other -> true
        other !is CharSequence -> false
        else -> string.toString() == other.toString()
    }

    override fun hashCode() = string.toString().hashCode()

    override fun toString() = string.toString()
}
