package core.writer

import core.exceptions.ExInfoException
import core.reader.Reader
import core.scm.Cons
import core.scm.Symbol
import java.util.regex.Pattern

object Writer {

    private val CODEPOINTS = HashMap<Char, String>()

    init {
        Reader.NAMED_CHARS.forEach { key, value -> CODEPOINTS.put(value, key) }
    }

    private val UNESCAPED = HashMap<Char, Char>()

    init {
        UNESCAPED.put('\t', 't')
        UNESCAPED.put('\b', 'b')
        UNESCAPED.put('\r', 'r')
        UNESCAPED.put('\n', 'n')
        UNESCAPED.put('\"', '"')
        UNESCAPED.put('\\', '\\')
    }

    @JvmStatic fun write(o: Any?): String {
        if (o == null)             return "nil"
        if (o is Boolean)          return if (o) "#t" else "#f"
        if (o is Symbol)           return o.write()
        if (o is Class<*>)         return o.write()
        if (o is List<*>)          return o.write()
        if (o is Number)           return o.write()
        if (o is CharSequence)     return o.write()
        if (o is Char)             return o.write()
        if (o is Pattern)          return o.write()
        if (o is Throwable)        return o.write()
        if (o is Map<*, *>)        return o.write()
        if (o is Map.Entry<*, *>)  return o.write()
        if (o is Set<*>)           return o.write()
        return o.toString()
    }

    fun Class<*>.write(): String {
        return "#<class:$name>"
    }

    private fun Pattern.write(): String {
        return "#\"${this}\""
    }

    private fun List<*>.write(): String {
        return Cons.toString(this)
    }

    private fun Symbol.write(): String {
        return when {
            isEscape -> '|' + toString() + '|'
            else -> toString()
        }
    }

    private fun Number.write(): String {
        if (this is Double) {
            if (java.lang.Double.isNaN(toDouble())) {
                return "+nan.0"
            } else if (this.toDouble() == java.lang.Double.POSITIVE_INFINITY) {
                return "+inf.0"
            } else if (this.toDouble() == java.lang.Double.NEGATIVE_INFINITY) {
                return "-inf.0"
            }
        }
        if (this is Float) {
            if (java.lang.Float.isNaN(toFloat())) {
                return "+nan.0"
            } else if (this.toFloat() == java.lang.Float.POSITIVE_INFINITY) {
                return "+inf.0"
            } else if (this.toFloat() == java.lang.Float.NEGATIVE_INFINITY) {
                return "-inf.0"
            }
        }
        return toString()
    }

    private fun CharSequence.write(): String {
        /* Unescape Strings */
        val length = length
        val sb = StringBuilder(length + 2)
        sb.append('"')
        for (i in 0..length - 1) {
            val c = this[i]
            val character = UNESCAPED[c]
            when (character) {
                null -> sb.append(c)
                else -> sb.append('\\').append(character)
            }
        }
        sb.append('"')
        return sb.toString()
    }

    private fun Char?.write(): String {
        /* Check named characters */
        val codepoint = CODEPOINTS[this]
        return if (codepoint == null) "#\\" + this!! else "#\\" + codepoint
    }

    private fun Map.Entry<*, *>.write(): String {
        return "[${write(key)} ${write(value)}]"
    }

    private fun Map<*, *>.write(): String {
        if (isEmpty()) {
            return "{}"
        }
        val sb = StringBuilder().append('{')
        var first = true
        for ((key, value) in this) {
            when {
                first -> first = false
                else -> sb.append(", ")
            }
            sb.append(if (key === this@Writer) "(this hashmap)" else write(key))
            sb.append(' ')
            sb.append(if (value === this@Writer) "(this hashmap)" else write(value))
        }
        return sb.append('}').toString()
    }

    private fun Set<*>.write(): String {
        if (isEmpty()) {
            return "#{}"
        }
        val sb = StringBuilder().append("#{")
        var first = true
        for (e in this) {
            when {
                first -> first = false
                else -> sb.append(' ')
            }
            sb.append(if (e === this@Writer) "(this set)" else write(e))
        }
        return sb.append('}').toString()
    }

    private fun Throwable.write(): String {
        when {
            this is ExInfoException -> return toString()
            else -> return "#<error:" + javaClass.name + ":" + (if (message == null) "" else message) + ">"
        }
    }
}
