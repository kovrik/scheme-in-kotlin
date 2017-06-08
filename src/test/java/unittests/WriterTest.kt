package unittests

import core.scm.Cons
import core.writer.Writer
import org.junit.Test


import core.scm.Cons.Companion.EMPTY
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
        assertEquals("()", Writer.write(EMPTY))
        assertEquals("()", Writer.write(Cons.list<Any>()))
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
    fun testWriteEscape() {
        assertEquals("\"\\t\\b\\r\\n\\\\\\\"\"", Writer.write("\t\b\r\n\\\""))
    }
}
