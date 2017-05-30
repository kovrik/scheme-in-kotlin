package core.scm

/**
 * Mutable Scheme String

 * Java String is immutable, StringBuilder is mutable.
 * But both classes are final and cannot be extended.
 * This MutableString class holds a mutable StringBuilder instance
 * and delegates it all string operations.
 */
class MutableString : INamed, CharSequence {

    private val string: StringBuilder

    override val length: Int
        get() = this.string.length

    constructor() {
        this.string = StringBuilder()
    }

    constructor(string: String) {
        this.string = StringBuilder(string)
    }

    constructor(length: Int) {
        this.string = StringBuilder(length)
    }

    fun append(c: Any?): MutableString {
        this.string.append(c)
        return this
    }

    override fun get(index: Int): Char {
        return string[index]
    }

    override fun subSequence(startIndex: Int, endIndex: Int) = string.subSequence(startIndex, endIndex)

    fun setCharAt(index: Int, ch: Char) = this.string.setCharAt(index, ch)

    fun clear() = string.setLength(0)

    override val name
        get() = string.toString()

    override fun toString() = string.toString()

    override fun equals(other: Any?): Boolean {
        return when {
            this === other -> true
            other !is CharSequence -> false
            else -> string.toString() == other.toString()
        }
    }

    override fun hashCode() = string.toString().hashCode()
}
