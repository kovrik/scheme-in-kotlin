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

    private val UNESCAPED = hashMapOf(
            '\t' to 't',
            '\b' to 'b',
            '\r' to 'r',
            '\n' to 'n',
            '\"' to '"',
            '\\' to '\\'
    )

    @JvmStatic fun write(o: Any?): String {
        when (o) {
            null               -> return "nil"
            is Boolean         -> return if (o) "#t" else "#f"
            is Symbol          -> return o.write()
            is Class<*>        -> return o.write()
            is List<*>         -> return o.write()
            is Number          -> return o.write()
            is CharSequence    -> return o.write()
            is Char            -> return o.write()
            is Pattern         -> return o.write()
            is Throwable       -> return o.write()
            is Map<*, *>       -> return o.write()
            is Map.Entry<*, *> -> return o.write()
            is Set<*>          -> return o.write()
            else               -> return o.toString()
        }
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
            } else if (this == java.lang.Double.POSITIVE_INFINITY) {
                return "+inf.0"
            } else if (this == java.lang.Double.NEGATIVE_INFINITY) {
                return "-inf.0"
            }
        }
        if (this is Float) {
            if (java.lang.Float.isNaN(toFloat())) {
                return "+nan.0"
            } else if (this == java.lang.Float.POSITIVE_INFINITY) {
                return "+inf.0"
            } else if (this == java.lang.Float.NEGATIVE_INFINITY) {
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
