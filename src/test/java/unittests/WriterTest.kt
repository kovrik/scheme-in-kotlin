package unittests

import core.writer.Writer
import org.junit.Test
import org.junit.Assert.assertEquals

class WriterTest {

    @Test
    fun testWriteString() {
        assertEquals("\"test string\"", Writer.write("test string"))
        assertEquals("\"\"", Writer.write(""))
        assertEquals("\"(1 2 3)\"", Writer.write("(1 2 3)"))
    }

    @Test
    fun testWriteChar() {
        assertEquals("#\\a", Writer.write('a'))
        assertEquals("#\\b", Writer.write('b'))
        assertEquals("#\\Z", Writer.write('Z'))
    }

    @Test
    fun testWriteNil() {
        assertEquals("nil", Writer.write(null as Any?))
        assertEquals("()", Writer.write(listOf<Any?>()))
        assertEquals("()", Writer.write(emptyList<Any?>()))
        assertEquals("()", Writer.write(listOf(1, 2, 3).subList(3, 3)))
    }

    @Test
    fun testWriteList() {
        assertEquals("(1 2 3)", Writer.write(listOf(1, 2, 3).subList(0, 3)))
        assertEquals("(1 2 3 4)", Writer.write(listOf(1, 2, 3, 4)))
        assertEquals("(#\\a #\\b #\\c)", Writer.write(listOf('a', 'b', 'c')))
        assertEquals("(\"test\" \"string\")", Writer.write(listOf("test", "string")))
        assertEquals("(nil nil nil)", Writer.write(listOf(null, null, null)))
    }

    @Test
    fun testWriteMap() {
        assertEquals("{}", Writer.write(mapOf<Nothing, Nothing>()))
        assertEquals("{\"test\" 1}", Writer.write(mapOf(Pair("test", 1))))
    }

    @Test
    fun testWriteSet() {
        assertEquals("#{}", Writer.write(setOf<Nothing>()))
        assertEquals("#{1}", Writer.write(setOf(1)))
        assertEquals("#{#\\a}", Writer.write(setOf('a')))
        assertEquals("#{\"test\"}", Writer.write(setOf("test")))
        assertEquals("#{nil}", Writer.write(setOf(null, null, null)))
    }

    @Test
    fun testWriteEscape() {
        assertEquals("\"\\t\\b\\r\\n\\\\\\\"\"", Writer.write("\t\b\r\n\\\""))
    }

    @Test
    fun testWriteThrowable() {
        assertEquals("#<error:java.lang.NullPointerException>", Writer.write(NullPointerException()))
        assertEquals("#<error:java.lang.NullPointerException:BOOM>", Writer.write(NullPointerException("BOOM")))
    }

    @Test
    fun testWriteArray() {
        assertEquals("[]", Writer.write(byteArrayOf()))
        assertEquals("[]", Writer.write(shortArrayOf()))
        assertEquals("[]", Writer.write(intArrayOf()))
        assertEquals("[]", Writer.write(longArrayOf()))
        assertEquals("[]", Writer.write(doubleArrayOf()))
        assertEquals("[]", Writer.write(floatArrayOf()))
        assertEquals("[]", Writer.write(charArrayOf()))
        assertEquals("[]", Writer.write(booleanArrayOf()))
        assertEquals("[]", Writer.write(arrayOf<Any?>()))
        assertEquals("[1, 2, 3]", Writer.write(byteArrayOf(1,2,3)))
        assertEquals("[1, 2, 3]", Writer.write(shortArrayOf(1,2,3)))
        assertEquals("[1, 2, 3]", Writer.write(intArrayOf(1,2,3)))
        assertEquals("[1, 2, 3]", Writer.write(longArrayOf(1L,2L,3L)))
        assertEquals("[1.0, 2.0, 3.0]", Writer.write(doubleArrayOf(1.0,2.0,3.0)))
        assertEquals("[1.0, 2.0, 3.0]", Writer.write(floatArrayOf(1.0f,2.0f,3.0f)))
        assertEquals("[#\\a, #\\b, #\\c]", Writer.write(charArrayOf('a', 'b', 'c')))
        assertEquals("[#t, #f, #t]", Writer.write(booleanArrayOf(true, false, true)))
        assertEquals("[nil, nil]", Writer.write(arrayOf<Any?>(null, null)))
    }

    // TODO Vector, Sequence, Pair, Regex, Thread, Map.Entry, Pattern, CharSequence, Number, Symbol, Keyword, Class, Boolean, Special Cases etc.
}
