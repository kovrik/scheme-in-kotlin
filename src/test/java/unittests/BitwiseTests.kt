package unittests

import org.junit.Test

import org.junit.Assert.assertEquals

class BitwiseTests : AbstractTest() {

    @Test
    fun testBitAnd() {
        assertEquals(-1L,   eval("(bit-and)", env))
        assertEquals(5L,    eval("(bit-and 5)", env))
        assertEquals(8L,    eval("(bit-and #b1100 #b1001)", env))
        assertEquals(8L,    eval("(bit-and 12 9)", env))
        assertEquals("108", eval("(Integer/toHexString (bit-and #x0108 #xffff))", env))
        assertEquals(195L,  eval("(bit-and 235 199)", env))
    }

    @Test
    fun testBitAndNot() {
        assertEquals(4L,    eval("(bit-and-not #b1100 #b1001)", env))
        assertEquals("100", eval("(Integer/toBinaryString (bit-and-not #b1100 #b1010))", env))
    }

    @Test
    fun testBitClear() {
        assertEquals(3L, eval("(bit-clear #b1011 3)", env))
        assertEquals(3L, eval("(bit-clear 11 3)", env))
        assertEquals(2147479551L, eval("(bit-clear Integer/MAX_VALUE 12)", env))
    }

    @Test
    fun testBitFlip() {
        assertEquals(15L, eval("(bit-flip #b1011 2)", env))
        assertEquals(11L, eval("(bit-flip #b1111 2)", env))
        assertEquals(2147450879L, eval("(bit-flip Integer/MAX_VALUE 15)", env))
    }

    @Test
    fun testBitNot() {
        assertEquals(-8L, eval("(bit-not #b0111)", env))
        assertEquals(7L,  eval("(bit-not #b-1000)", env))
    }

    @Test
    fun testBitOr() {
        assertEquals(0L,     eval("(bit-or)", env))
        assertEquals(5L,     eval("(bit-or 5)", env))
        assertEquals(13L,    eval("(bit-or #b1100 #b1001)", env))
        assertEquals(13L,    eval("(bit-or 12 9)", env))
        assertEquals("1110", eval("(Integer/toBinaryString (bit-or #b1100 #b1010))", env))
    }

    @Test
    fun testBitSet() {
        assertEquals(15L, eval("(bit-set #b1011 2)", env))
        assertEquals(15L, eval("(bit-set 11 2)", env))
        assertEquals(java.lang.Long.valueOf("-9223372036854775808"), eval("(bit-set 0 63)", env))
    }

    @Test
    fun testBitShiftLeft() {
        assertEquals(1024L, eval("(bit-shift-left 1 10)", env))
        assertEquals(52L,   eval("(bit-shift-left #b1101 2)", env))
    }

    @Test
    fun testBitShiftRight() {
        assertEquals(13L, eval("(bit-shift-right #b1101 0)", env))
        assertEquals(6L,  eval("(bit-shift-right #b1101 1)", env))
        assertEquals(3L,  eval("(bit-shift-right #b1101 2)", env))
        assertEquals(1L,  eval("(bit-shift-right #b1101 3)", env))
        assertEquals(0L,  eval("(bit-shift-right #b1101 4)", env))
    }

    @Test
    fun testBitTest() {
        assertEquals(true,  eval("(bit-test #b1001 0)", env))
        assertEquals(true,  eval("(bit-test #b1001 3)", env))
        assertEquals(false, eval("(bit-test #b1001 1)", env))
        assertEquals(false, eval("(bit-test #b1001 2)", env))
        assertEquals(false, eval("(bit-test #b1001 7)", env))
    }

    @Test
    fun testBitXor() {
        assertEquals(0L,    eval("(bit-xor)", env))
        assertEquals(5L,    eval("(bit-xor 5)", env))
        assertEquals(5L,    eval("(bit-xor #b1100 #b1001)", env))
        assertEquals("110", eval("(Integer/toBinaryString (bit-xor #b1100 #b1010))", env))
    }
}
