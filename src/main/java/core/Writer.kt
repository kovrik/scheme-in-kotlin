package core

import core.exceptions.ExInfoException
import core.procedures.Arity
import core.reader.Reader
import core.scm.MutablePair
import core.scm.Symbol
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.Job
import java.util.*
import java.util.regex.Pattern

object Writer {

    private val CODEPOINTS = HashMap<Char, String>().apply {
        Reader.NAMED_CHARS.forEach { (key, value) -> put(value, key) }.apply { put('\n', "newline") }
    }

    private val UNESCAPED = hashMapOf('\t' to 't', '\b' to 'b', '\r' to 'r', '\n' to 'n', '\"' to '"', '\\' to '\\')

    fun write(o: Any?): String = when (o) {
        null                    -> "nil"
        Unit                    -> "#<void>"
        is Boolean              -> if (o) "#t" else "#f"
        is Symbol               -> o.write()
        is List<*>              -> o.write()
        is Class<*>             -> o.write()
        is Pair<*, *>           -> o.write()
        is MutablePair<*, *>    -> o.write()
        is Number               -> o.write()
        is Sequence<*>          -> o.write()
        is CharSequence         -> o.write()
        is Char                 -> o.write()
        is Pattern              -> o.write()
        is Regex                -> o.write()
        is Throwable            -> o.write()
        is Map<*, *>            -> o.write()
        is Map.Entry<*, *>      -> o.write()
        is Set<*>               -> o.write()
        is ByteArray            -> o.write()
        is ShortArray           -> o.write()
        is IntArray             -> o.write()
        is LongArray            -> o.write()
        is DoubleArray          -> o.write()
        is FloatArray           -> o.write()
        is CharArray            -> o.write()
        is BooleanArray         -> o.write()
        is Array<*>             -> o.write()
        is Thread               -> o.write()
        is Job                  -> o.write()
        is Deferred<*>          -> o.write()
        is Arity                -> o.write()
        else                    -> o.toString()
    }

    private fun Class<*>.write() = when {
        isArray -> "#<class:$simpleName>"
        else    -> "#<class:$name>"
    }

    private fun Thread.write() = when {
        name.isEmpty() -> "#<thread>"
        else           -> "#<thread:$name>"
    }

    private fun Job.write() = when {
        isActive    -> "#<coroutine:active>"
        isCancelled -> "#<coroutine:cancelled>"
        else        -> "#<coroutine:done>"
    }

    private fun Deferred<*>.write() = when {
        isActive    -> "#<coroutine:active>"
        isCancelled -> "#<coroutine:cancelled>"
        else        -> "#<coroutine:done(${write(getCompleted())})>"
    }

    private fun Arity.write() = "#<arity:${javaClass.simpleName}(${toString()})>"

    private fun Pattern.write() = "#\"${this}\""

    private fun Regex.write() = "#\"${this}\""

    /* Use this method to print all lists */
    private fun List<*>.write() = joinToString(prefix = "(", separator = " ", postfix = ")", transform = Writer::write)

    private fun Pair<*, *>.write() = StringBuilder("(").apply {
        append(write(this@write.first))
        var cdr = this@write.second
        while (cdr is Pair<*, *>) {
            append(' ').append(write(cdr.first))
            cdr = cdr.second
        }
        /* Dotted notation */
        append(" . ").append(write(cdr)).append(')')
    }.toString()

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

    private fun Sequence<*>.write() = joinToString(prefix = "(", separator = " ", postfix = ")", transform = Writer::write)

    private fun CharSequence.write() = StringBuilder(length + 2).append('"').apply {
        (0 until this@write.length).forEach {
            val c = this@write[it]
            when (val character = UNESCAPED[c]) {
                null -> append(c)
                else -> append('\\').append(character)
            }
        }
    }.append('"').toString()

    /* Check named characters */
    private fun Char.write() = CODEPOINTS[this]?.let { "#\\$it" } ?: "#\\$this"

    private fun Map<*, *>.write() = entries.joinToString(prefix = "{", separator = ", ", postfix = "}",
                                                         transform = { "${write(it.key)} ${write(it.value)}" })

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
