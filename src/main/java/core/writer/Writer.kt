package core.writer

import core.exceptions.ExInfoException
import core.reader.Reader
import core.scm.Cons
import core.scm.Symbol
import java.util.regex.Pattern

object Writer {

    private val CODEPOINTS = HashMap<Char, String>().apply {
        Reader.NAMED_CHARS.forEach { key, value -> put(value, key) }.apply { put('\n', "newline") }
    }

    private val UNESCAPED = hashMapOf('\t' to 't', '\b' to 'b', '\r' to 'r', '\n' to 'n', '\"' to '"', '\\' to '\\')

    fun write(o: Any?): String = when (o) {
        null               -> "nil"
        is Unit            -> "#<void>"
        is Boolean         -> if (o) "#t" else "#f"
        is Symbol          -> o.write()
        is Class<*>        -> o.write()
        is List<*>         -> o.write()
        is Number          -> o.write()
        is CharSequence    -> o.write()
        is Char            -> o.write()
        is Pattern         -> o.write()
        is Regex           -> o.write()
        is Throwable       -> o.write()
        is Map<*, *>       -> o.write()
        is Map.Entry<*, *> -> o.write()
        is Set<*>          -> o.write()
        else               -> o.toString()
    }

    private fun Class<*>.write() = "#<class:$name>"

    private fun Pattern.write() = "#\"${this}\""

    private fun Regex.write() = "#\"${this}\""

    private fun List<*>.write() = Cons.toString(this)

    private fun Map.Entry<*, *>.write() = "[${write(key)} ${write(value)}]"

    private fun Symbol.write() = when {
        isEscape -> '|' + toString() + '|'
        else -> toString()
    }

    private fun Number.write(): String = when (this) {
        Double.NaN               -> "+nan.0"
        Double.POSITIVE_INFINITY -> "+inf.0"
        Double.NEGATIVE_INFINITY -> "-inf.0"
        Float.NaN                -> "+nan.0"
        Float.POSITIVE_INFINITY  -> "+inf.0"
        Float.NEGATIVE_INFINITY  -> "-inf.0"
        else                     -> toString()
    }

    private fun CharSequence.write(): String {
        /* Unescape Strings */
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
        return sb.append('"').toString()
    }

    /* Check named characters */
    private fun Char?.write() = CODEPOINTS[this]?.let { "#\\$it" } ?: "#\\$this"

    private fun Map<*, *>.write(): String {
        if (isEmpty()) return "{}"
        val sb = StringBuilder().append('{')
        var first = true
        for ((key, value) in this) {
            when {
                first -> first = false
                else -> sb.append(", ")
            }
            sb.append(if (key === this) "(this hashmap)" else write(key))
            sb.append(' ')
            sb.append(if (value === this) "(this hashmap)" else write(value))
        }
        return sb.append('}').toString()
    }

    private fun Set<*>.write(): String {
        if (isEmpty()) return "#{}"
        val sb = StringBuilder().append("#{")
        var first = true
        for (e in this) {
            when {
                first -> first = false
                else -> sb.append(' ')
            }
            sb.append(if (e === this) "(this set)" else write(e))
        }
        return sb.append('}').toString()
    }

    private fun Throwable.write() = when (this) {
        is ExInfoException -> toString()
        else -> "#<error:" + javaClass.name + ":" + (if (message == null) "" else message) + ">"
    }
}
