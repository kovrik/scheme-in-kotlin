package core.writer

import core.exceptions.ExInfoException
import core.reader.Reader
import core.scm.Cons
import core.scm.Symbol
import java.util.*
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
        is ByteArray       -> o.write()
        is ShortArray      -> o.write()
        is IntArray        -> o.write()
        is LongArray       -> o.write()
        is DoubleArray     -> o.write()
        is FloatArray      -> o.write()
        is CharArray       -> o.write()
        is BooleanArray    -> o.write()
        is Array<*>        -> o.write()
        is Thread          -> o.write()
        else               -> o.toString()
    }

    private fun Class<*>.write() = when {
        isArray -> "#<class:$simpleName>"
        else    -> "#<class:$name>"
    }

    private fun Thread.write() = when {
        name.isEmpty() -> "#<thread>"
        else           -> "#<thread:$name>"
    }

    private fun Pattern.write() = "#\"${this}\""

    private fun Regex.write() = "#\"${this}\""

    private fun List<*>.write() = Cons.toString(this)

    private fun Map.Entry<*, *>.write() = "[${write(key)} ${write(value)}]"

    private fun Symbol.write() = when {
        escape -> '|' + toString() + '|'
        else   -> toString()
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

    private fun CharSequence.write() = StringBuilder(length + 2).apply {
        append('"')
        for (i in 0 until this@write.length) {
            val c = this@write[i]
            val character = UNESCAPED[c]
            when (character) {
                null -> append(c)
                else -> append('\\').append(character)
            }
        }
        append('"')
    }.toString()

    /* Check named characters */
    private fun Char.write() = CODEPOINTS[this]?.let { "#\\$it" } ?: "#\\$this"

    private fun Map<*, *>.write() = when {
        isEmpty() -> "{}"
        else -> StringBuilder("{").apply {
            var first = true
            for ((key, value) in this@write) {
                when {
                    first -> first = false
                    else  -> append(", ")
                }
                append(if (key === this@write) "(this hashmap)" else write(key))
                append(' ')
                append(if (value === this@write) "(this hashmap)" else write(value))
            }
            append('}')
        }.toString()
    }

    private fun Set<*>.write() = when {
        isEmpty() -> "#{}"
        else -> StringBuilder("#{").apply {
            var first = true
            for (e in this@write) {
                when {
                    first -> first = false
                    else -> append(' ')
                }
                append(if (e === this@write) "(this set)" else write(e))
            }
            append('}')
        }.toString()
    }

    private fun Throwable.write() = when (this) {
        is ExInfoException -> toString()
        else -> "#<error:" + javaClass.name + ":" + (if (message == null) "" else message) + ">"
    }

    private fun ByteArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun ShortArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun IntArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun LongArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun DoubleArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun FloatArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun CharArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun BooleanArray.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }

    private fun Array<*>.write() = when {
        isEmpty() -> "[]"
        else -> StringBuilder().append(this@write.joinToString(prefix = "[", separator = ", ", postfix = "]",
                                                               transform = { write(it) })).toString()
    }
}
