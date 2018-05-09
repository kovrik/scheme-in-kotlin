package core

import core.exceptions.ExInfoException
import core.procedures.predicates.Predicate
import core.reader.Reader
import core.scm.Cons
import core.scm.MutablePair
import core.scm.Symbol
import java.util.*
import java.util.regex.Pattern

object Writer {

    private val CODEPOINTS = HashMap<Char, String>().apply {
        Reader.NAMED_CHARS.forEach { key, value -> put(value, key) }.apply { put('\n', "newline") }
    }

    private val UNESCAPED = hashMapOf('\t' to 't', '\b' to 'b', '\r' to 'r', '\n' to 'n', '\"' to '"', '\\' to '\\')

    fun write(o: Any?): String = when (o) {
        null                 -> "nil"
        Unit                 -> "#<void>"
        is Boolean           -> if (o) "#t" else "#f"
        is Symbol            -> o.write()
        is List<*>           -> o.write()
        is Class<*>          -> o.write()
        is Pair<*, *>        -> o.write()
        is MutablePair<*, *> -> o.write()
        is Number            -> o.write()
        is Sequence<*>       -> o.write()
        is CharSequence      -> o.write()
        is Char              -> o.write()
        is Pattern           -> o.write()
        is Regex             -> o.write()
        is Throwable         -> o.write()
        is Map<*, *>         -> o.write()
        is Map.Entry<*, *>   -> o.write()
        is Set<*>            -> o.write()
        is ByteArray         -> o.write()
        is ShortArray        -> o.write()
        is IntArray          -> o.write()
        is LongArray         -> o.write()
        is DoubleArray       -> o.write()
        is FloatArray        -> o.write()
        is CharArray         -> o.write()
        is BooleanArray      -> o.write()
        is Array<*>          -> o.write()
        is Thread            -> o.write()
        else                 -> o.toString()
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

    /* Use this method to print all lists */
    private fun List<*>.write() = when {
        this.isEmpty() -> "()"
        Predicate.isProperList(this) -> StringBuilder("(").apply {
            for (i in 0..this@write.size - 2) {
                append(if (this@write[i] === this@write) "(this list)" else write(this@write[i])).append(' ')
            }
            append(if (this@write.last() === this@write) "(this list)" else write(this@write.last())).append(')')
        }.toString()
        else -> StringBuilder("(").apply {
            append(write(this@write.first()))
            var cdr = this@write.last()
            while (cdr is Cons<*>) {
                append(' ').append(write(cdr.first()))
                cdr = cdr.last()
            }
            /* Dotted notation */
            append(" . ").append(write(cdr)).append(')')
        }.toString()
    }

    private fun Pair<*, *>.write() = "(pair ${write(first)} ${write(second)})"

    private fun MutablePair<*, *>.write() = "(mcons ${write(first)} ${write(second)})"

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

    private fun Sequence<*>.write() = toList().write()

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

    private fun Map<*, *>.write() = entries.joinToString(prefix = "{", separator = ", ", postfix = "}",
                                                         transform = { "${Writer.write(it.key)} ${Writer.write(it.value)}" })

    private fun Set<*>.write() = joinToString(prefix = "#{", separator = " ", postfix = "}", transform = Writer::write)

    private fun Throwable.write() = when (this) {
        is ExInfoException -> toString()
        else -> "#<error:" + javaClass.name + (if (message == null) "" else ":$message") + ">"
    }

    private fun ByteArray.write()    = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun ShortArray.write()   = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun IntArray.write()     = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun LongArray.write()    = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun DoubleArray.write()  = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun FloatArray.write()   = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun CharArray.write()    = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun BooleanArray.write() = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
    private fun Array<*>.write()     = joinToString(prefix = "[", separator = ", ", postfix = "]", transform = Writer::write)
}
