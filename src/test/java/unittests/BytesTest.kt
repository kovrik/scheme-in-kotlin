package unittests

import core.exceptions.WrongTypeException
import core.scm.Cons
import core.scm.MutableString
import core.scm.Symbol
import core.scm.Vector
import core.writer.Writer
import org.junit.Test

import org.junit.Assert.assertEquals
import kotlin.test.assertTrue

class BytesTest : AbstractTest() {

    @Test
    fun testBytes() {
        assertTrue(byteArrayOf() contentEquals eval("(bytes)", env) as ByteArray)
        assertTrue(byteArrayOf(1) contentEquals eval("(bytes 1)", env) as ByteArray)
        assertTrue(byteArrayOf(1, 2, 3) contentEquals eval("(bytes 1 2 3)", env) as ByteArray)
    }

    @Test
    fun testMakeBytes() {
        assertTrue(byteArrayOf() contentEquals eval("(make-bytes 0)", env) as ByteArray)
        assertTrue(byteArrayOf(1) contentEquals eval("(make-bytes 1 1)", env) as ByteArray)
        assertTrue(byteArrayOf(1, 1, 1) contentEquals eval("(make-bytes 3 1)", env) as ByteArray)
    }

    @Test
    fun testBytesAppend() {
        assertTrue(byteArrayOf() contentEquals eval("(bytes-append (bytes) (bytes))", env) as ByteArray)
        assertTrue(byteArrayOf(1, 2, 3, 3, 4, 5) contentEquals eval("(bytes-append (bytes 1 2 3) (bytes 3 4 5))", env) as ByteArray)
    }

    @Test
    fun testBytesFill() {
        assertTrue(byteArrayOf() contentEquals eval("(bytes-fill! (make-bytes 0) 1)", env) as ByteArray)
        assertTrue(byteArrayOf(1, 1, 1) contentEquals eval("(bytes-fill! (make-bytes 3) 1)", env) as ByteArray)
        assertTrue(byteArrayOf(1, 1, 1) contentEquals eval("(bytes-fill! (make-bytes 3 2) 1)", env) as ByteArray)
    }

    @Test
    fun testBytesLength() {
        assertEquals(0L, eval("(bytes-length (bytes))", env))
        assertEquals(3L, eval("(bytes-length (bytes 1 2 3))", env))
        assertEquals(5,  eval("(count (bytes 1 2 3 4 5))", env))
    }

    @Test
    fun testBytesRef() {
        assertEquals(1.toByte(), eval("(bytes-ref (bytes 1 2 3) 0)", env))
        assertEquals(2.toByte(), eval("(bytes-ref (bytes 1 2 3) 1)", env))
        assertEquals(3.toByte(), eval("(bytes-ref (bytes 1 2 3) 2)", env))
    }

    @Test
    fun testBytesSet() {
        assertEquals(12.toByte(), eval("(let ((b (bytes 1 2 3))) (bytes-set! b 0 12) (bytes-ref b 0))", env))
    }

    @Test
    fun testBytesToList() {
        assertEquals(Cons.list<Byte>(), eval("(bytes->list (bytes))", env))
        assertEquals(Cons.list<Byte>(1, 2, 3), eval("(bytes->list (bytes 1 2 3))", env))
    }

    @Test
    fun testListToBytes() {
        assertTrue(byteArrayOf() contentEquals  eval("(list->bytes '())", env) as ByteArray)
        assertTrue(byteArrayOf(1, 2, 3) contentEquals  eval("(list->bytes '(1 2 3))", env) as ByteArray)
    }

    @Test
    fun testBytesToString() {
        assertEquals("", eval("(bytes->string (bytes))", env))
        assertEquals("Apple", eval("(bytes->string (bytes 65 112 112 108 101))", env))
    }
}
