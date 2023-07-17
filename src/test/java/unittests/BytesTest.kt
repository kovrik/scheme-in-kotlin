package unittests

import org.junit.Test

import org.junit.Assert.assertEquals
import kotlin.test.assertNotEquals
import kotlin.test.assertTrue

class BytesTest : AbstractTest() {

    @Test
    fun testBytes() {
        assertTrue(byteArrayOf() contentEquals eval("(bytes)") as ByteArray)
        assertTrue(byteArrayOf(1) contentEquals eval("(bytes 1)") as ByteArray)
        assertTrue(byteArrayOf(1, 2, 3) contentEquals eval("(bytes 1 2 3)") as ByteArray)
    }

    @Test
    fun testMakeBytes() {
        assertTrue(byteArrayOf() contentEquals eval("(make-bytes 0)") as ByteArray)
        assertTrue(byteArrayOf(1) contentEquals eval("(make-bytes 1 1)") as ByteArray)
        assertTrue(byteArrayOf(1, 1, 1) contentEquals eval("(make-bytes 3 1)") as ByteArray)
    }

    @Test
    fun testBytesAppend() {
        assertTrue(byteArrayOf() contentEquals eval("(bytes-append (bytes) (bytes))") as ByteArray)
        assertTrue(byteArrayOf(1, 2, 3, 3, 4, 5) contentEquals eval("(bytes-append (bytes 1 2 3) (bytes 3 4 5))") as ByteArray)
    }

    @Test
    fun testBytesFill() {
        assertTrue(byteArrayOf() contentEquals eval("(bytes-fill! (make-bytes 0) 1)") as ByteArray)
        assertTrue(byteArrayOf(1, 1, 1) contentEquals eval("(bytes-fill! (make-bytes 3) 1)") as ByteArray)
        assertTrue(byteArrayOf(1, 1, 1) contentEquals eval("(bytes-fill! (make-bytes 3 2) 1)") as ByteArray)
    }

    @Test
    fun testBytesLength() {
        assertEquals(0L, eval("(bytes-length (bytes))"))
        assertEquals(3L, eval("(bytes-length (bytes 1 2 3))"))
        assertEquals(5,  eval("(count (bytes 1 2 3 4 5))"))
    }

    @Test
    fun testBytesRef() {
        assertEquals(1.toByte(), eval("(bytes-ref (bytes 1 2 3) 0)"))
        assertEquals(2.toByte(), eval("(bytes-ref (bytes 1 2 3) 1)"))
        assertEquals(3.toByte(), eval("(bytes-ref (bytes 1 2 3) 2)"))
    }

    @Test
    fun testBytesSet() {
        assertEquals(12.toByte(), eval("(let ((b (bytes 1 2 3))) (bytes-set! b 0 12) (bytes-ref b 0))"))
    }

    @Test
    fun testBytesToList() {
        assertEquals(emptyList<Byte>(), eval("(bytes->list (bytes))"))
        assertEquals(listOf<Byte>(1, 2, 3), eval("(bytes->list (bytes 1 2 3))"))
    }

    @Test
    fun testListToBytes() {
        assertTrue(byteArrayOf() contentEquals  eval("(list->bytes '())") as ByteArray)
        assertTrue(byteArrayOf(1, 2, 3) contentEquals  eval("(list->bytes '(1 2 3))") as ByteArray)
    }

    @Test
    fun testBytesToString() {
        assertEquals("", eval("(bytes->string (bytes))"))
        assertEquals("Apple", eval("(bytes->string (bytes 65 112 112 108 101))"))
    }

    @Test
    fun testSubBytes() {
        assertTrue(byteArrayOf(1, 2, 3, 4) contentEquals eval("(subbytes (bytes 1 2 3 4) 0)") as ByteArray)
        assertTrue(byteArrayOf(2, 3, 4) contentEquals eval("(subbytes (bytes 1 2 3 4) 1)") as ByteArray)
        assertTrue(byteArrayOf(2, 3) contentEquals eval("(subbytes (bytes 1 2 3 4) 1 3)") as ByteArray)
    }

    @Test
    fun testStringToBytes() {
        assertTrue(byteArrayOf(65, 112, 112, 108, 101) contentEquals eval("(string->bytes \"Apple\")") as ByteArray)
        assertTrue(byteArrayOf(65, 112, 112, 108, 101) contentEquals eval("(string->bytes \"Apple\" \"US-ASCII\")") as ByteArray)
        assertTrue(byteArrayOf(-2, -1, 0, 65, 0, 112, 0, 112, 0, 108, 0, 101) contentEquals eval("(string->bytes \"Apple\" \"UTF-16\")") as ByteArray)
        assertEquals("λ Apple", eval("(bytes->string (string->bytes \"λ Apple\" \"UTF-16\") \"UTF-16\")"))
        assertEquals("λ Apple", eval("(bytes->string (string->bytes \"λ Apple\"))"))
        assertNotEquals("λ Apple", eval("(bytes->string (string->bytes \"λ Apple\" \"UTF-16\") \"UTF-8\")"))
    }
}
