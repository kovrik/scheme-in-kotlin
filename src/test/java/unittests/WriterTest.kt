package unittests

import core.environment.Environment
import core.scm.*
import core.scm.specialforms.If
import core.Writer
import org.junit.Test
import org.junit.Assert.assertEquals
import java.math.BigDecimal
import java.math.BigInteger
import java.util.regex.Pattern

class WriterTest {

    @Test
    fun testWriteString() {
        assertEquals("\"test string\"", Writer.write("test string"))
        assertEquals("\"\"", Writer.write(""))
        assertEquals("\"(1 2 3)\"", Writer.write("(1 2 3)"))
        assertEquals("\"12ሴ\\\"\\n\"", Writer.write("12\u1234\"\n"))
    }

    @Test
    fun testWriteChar() {
        assertEquals("#\\a", Writer.write('a'))
        assertEquals("#\\b", Writer.write('b'))
        assertEquals("#\\Z", Writer.write('Z'))
        assertEquals("#\\ሴ", Writer.write('\u1234'))
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

    @Test
    fun testWriteVector() {
        assertEquals("[]", Writer.write(Vector.EMPTY))
        assertEquals("[]", Writer.write(MutableVector.EMPTY))
        assertEquals("[#t #f #t]", Writer.write(Vector(arrayOf(true, false, true))))
        assertEquals("[#t #f #t]", Writer.write(MutableVector(arrayOf(true, false, true))))
    }

    @Test
    fun testWriteSequence() {
        assertEquals("()", Writer.write(emptySequence<Nothing>()))
        assertEquals("(#t #f #t)", Writer.write(listOf(true, false, true).asSequence()))
    }

    @Test
    fun testWriteThread() {
        assertEquals("#<thread:test>", Writer.write(Thread("test")))
    }

    @Test
    fun testWriteClass() {
        assertEquals("#<class:java.lang.String>", Writer.write(String::class.java))
        assertEquals("#<class:int[]>", Writer.write(IntArray::class.java))
    }

    @Test
    fun testWriteSymbol() {
        assertEquals("test", Writer.write(Symbol.intern("test")))
        assertEquals("|'|", Writer.write(Symbol.intern("'")))
        assertEquals("if", Writer.write(If.symbol))
        assertEquals("#<procedure>", Writer.write(Closure(emptyList(), Any(), Environment(0, null), true)))
    }

    @Test
    fun testWriteKeyword() {
        assertEquals(":test", Writer.write(Keyword.intern("test")))
        assertEquals(":'", Writer.write(Keyword.intern("'")))
    }

    @Test
    fun testWriteBoolean() {
        assertEquals("#t", Writer.write(true))
        assertEquals("#f", Writer.write(false))
    }

    @Test
    fun testWritePattern() {
        assertEquals("#\"abc\"", Writer.write(Pattern.compile("abc")))
    }

    @Test
    fun testWriteRegex() {
        assertEquals("#\"abc\"", Writer.write(Regex("abc")))
    }

    @Test
    fun testWriteMapEntry() {
        assertEquals("[\"test\" #t]", Writer.write(mapOf(Pair("test", true)).entries.first()))
    }

    @Test
    fun testWritePair() {
        assertEquals("(pair 1 #t)", Writer.write(Pair(1, true)))
        assertEquals("(mcons 1 #t)", Writer.write(MutablePair(1, true)))
    }

    @Test
    fun testWriteNumber() {
        assertEquals("+nan.0", Writer.write(Double.NaN))
        assertEquals("+nan.0", Writer.write(Float.NaN))
        assertEquals("+inf.0", Writer.write(Double.POSITIVE_INFINITY))
        assertEquals("+inf.0", Writer.write(Float.POSITIVE_INFINITY))
        assertEquals("-inf.0", Writer.write(Double.NEGATIVE_INFINITY))
        assertEquals("-inf.0", Writer.write(Float.NEGATIVE_INFINITY))
        assertEquals("1", Writer.write(1))
        assertEquals("-1", Writer.write(-1))
        assertEquals("1", Writer.write(1L))
        assertEquals("-1", Writer.write(-1L))
        assertEquals("1.0", Writer.write(1.0))
        assertEquals("1.0", Writer.write(1.0f))
        assertEquals("-1.0", Writer.write(-1.0))
        assertEquals("-1.0", Writer.write(-1.0f))
        assertEquals("10", Writer.write(BigDecimal.TEN))
        assertEquals("10", Writer.write(BigInteger.TEN))
        assertEquals("0", Writer.write(BigRatio.ZERO))
        assertEquals("1", Writer.write(BigRatio.ONE))
        assertEquals("1/2", Writer.write(BigRatio.valueOf("4", "8")))
        assertEquals("-1/2", Writer.write(BigRatio.valueOf("-4", "8")))
        assertEquals("0+1i", Writer.write(BigComplex.I))
        assertEquals("-1+0i", Writer.write(BigComplex.I.times(BigComplex.I)))
    }
}
