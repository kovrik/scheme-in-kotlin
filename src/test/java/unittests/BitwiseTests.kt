package unittests

import org.junit.Test

import org.junit.Assert.assertEquals
import java.math.BigInteger

class BitwiseTests : AbstractTest() {

    @Test
    fun testBitAnd() {
        assertEquals(-1L,   eval("(bit-and)"))
        assertEquals(5L,    eval("(bit-and 5)"))
        assertEquals(8L,    eval("(bit-and #b1100 #b1001)"))
        assertEquals(8L,    eval("(bit-and 12 9)"))
        assertEquals("108", eval("(Integer/toHexString (bit-and #x0108 #xffff))"))
        assertEquals(195L,  eval("(bit-and 235 199)"))
    }

    @Test
    fun testBitAndNot() {
        assertEquals(4L,    eval("(bit-and-not #b1100 #b1001)"))
        assertEquals("100", eval("(Integer/toBinaryString (bit-and-not #b1100 #b1010))"))
    }

    @Test
    fun testBitClear() {
        assertEquals(3L, eval("(bit-clear #b1011 3)"))
        assertEquals(3L, eval("(bit-clear 11 3)"))
        assertEquals(2147479551L, eval("(bit-clear Integer/MAX_VALUE 12)"))
    }

    @Test
    fun testBitFlip() {
        assertEquals(15L, eval("(bit-flip #b1011 2)"))
        assertEquals(11L, eval("(bit-flip #b1111 2)"))
        assertEquals(2147450879L, eval("(bit-flip Integer/MAX_VALUE 15)"))
    }

    @Test
    fun testBitNot() {
        assertEquals(-8L, eval("(bit-not #b0111)"))
        assertEquals(7L,  eval("(bit-not #b-1000)"))
    }

    @Test
    fun testBitOr() {
        assertEquals(0L,     eval("(bit-or)"))
        assertEquals(5L,     eval("(bit-or 5)"))
        assertEquals(13L,    eval("(bit-or #b1100 #b1001)"))
        assertEquals(13L,    eval("(bit-or 12 9)"))
        assertEquals("1110", eval("(Integer/toBinaryString (bit-or #b1100 #b1010))"))
    }

    @Test
    fun testBitSet() {
        assertEquals(15L, eval("(bit-set #b1011 2)"))
        assertEquals(15L, eval("(bit-set 11 2)"))
        assertEquals(java.lang.Long.valueOf("-9223372036854775808"), eval("(bit-set 0 63)"))
    }

    @Test
    fun testBitShiftLeft() {
        assertEquals(1024L, eval("(bit-shift-left 1 10)"))
        assertEquals(18014398509481984L, eval("(bit-shift-left 1 -10)"))
        assertEquals(-1024L, eval("(bit-shift-left -1 10)"))
        assertEquals(-18014398509481984L, eval("(bit-shift-left -1 -10)"))
        assertEquals(52L, eval("(bit-shift-left #b1101 2)"))
    }

    @Test
    fun testBitShiftRight() {
        assertEquals(13L, eval("(bit-shift-right #b1101 0)"))
        assertEquals(6L,  eval("(bit-shift-right #b1101 1)"))
        assertEquals(3L,  eval("(bit-shift-right #b1101 2)"))
        assertEquals(1L,  eval("(bit-shift-right #b1101 3)"))
        assertEquals(0L,  eval("(bit-shift-right #b1101 4)"))
        assertEquals(0L,  eval("(bit-shift-right #b1101 4)"))
        assertEquals(0L,  eval("(bit-shift-right 1  10)"))
        assertEquals(0L,  eval("(bit-shift-right 1 -10)"))
        assertEquals(-1L, eval("(bit-shift-right -1  10)"))
        assertEquals(-1L, eval("(bit-shift-right -1 -10)"))
    }

    @Test
    fun testArithmeticShift() {
        assertEquals(1024L, eval("(arithmetic-shift 1 10)"))
        assertEquals(52L,   eval("(arithmetic-shift #b1101 2)"))
        assertEquals(0L,    eval("(arithmetic-shift 1 -10)"))
        assertEquals(0L,    eval("(arithmetic-shift 1 -10)"))
        assertEquals(256L,  eval("(arithmetic-shift 1024 -2)"))
        assertEquals(128L,  eval("(arithmetic-shift 1024 -3)"))
        assertEquals(0L,    eval("(arithmetic-shift 1024 -30)"))
        assertEquals(BigInteger.valueOf(256L),  eval("(arithmetic-shift (bigint 1024) -2)"))
        assertEquals(BigInteger.valueOf(128L),  eval("(arithmetic-shift (bigint 1024) -3)"))
        assertEquals(BigInteger.valueOf(1024L), eval("(arithmetic-shift (bigint 1)  10)"))
        assertEquals(BigInteger.valueOf(0L),    eval("(arithmetic-shift (bigint 1) -10)"))
    }

    @Test
    fun testBitTest() {
        assertEquals(true,  eval("(bit-test #b1001 0)"))
        assertEquals(true,  eval("(bit-test #b1001 3)"))
        assertEquals(false, eval("(bit-test #b1001 1)"))
        assertEquals(false, eval("(bit-test #b1001 2)"))
        assertEquals(false, eval("(bit-test #b1001 7)"))
    }

    @Test
    fun testBitXor() {
        assertEquals(0L,    eval("(bit-xor)"))
        assertEquals(5L,    eval("(bit-xor 5)"))
        assertEquals(5L,    eval("(bit-xor #b1100 #b1001)"))
        assertEquals("110", eval("(Integer/toBinaryString (bit-xor #b1100 #b1010))"))
    }
}
