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

    fun append(c: Any): MutableString {
        this.string.append(c)
        return this
    }

    override fun get(index: Int): Char {
        return string[index]
    }

    override fun subSequence(startIndex: Int, endIndex: Int): CharSequence {
        return string.subSequence(startIndex, endIndex)
    }

    fun setCharAt(index: Int, ch: Char) {
        this.string.setCharAt(index, ch)
    }

    fun setLength(n: Int) {
        string.setLength(n)
    }

    fun clear() {
        string.setLength(0)
    }

    fun reverse(): MutableString {
        string.reverse()
        return this
    }

    override val name: String
        get() = string.toString()

    override fun toString(): String {
        return string.toString()
    }

    override fun equals(other: Any?): Boolean {
        when {
            this === other -> return true
            other !is CharSequence -> return false
            else -> return string.toString() == other.toString()
        }
    }

    override fun hashCode(): Int {
        return string.toString().hashCode()
    }
}
