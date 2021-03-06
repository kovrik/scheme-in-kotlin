package unittests

import core.exceptions.IllegalSyntaxException
import core.reader.StringReader
import core.scm.Complex
import core.scm.Ratio
import core.scm.Keyword
import core.scm.MutableVector
import core.scm.Symbol
import core.scm.Syntax
import core.scm.specialforms.Quasiquote
import core.scm.specialforms.Quote
import org.junit.Assert.*
import org.junit.Test
import java.math.BigDecimal
import java.math.BigInteger
import java.util.*
import java.util.regex.Pattern

class ReaderTest : AbstractTest() {

    private val reader = StringReader()

    @Test
    fun testReadNumbers() {
        assertEquals(1L, reader.readOne("1"))
        assertEquals(12345L, reader.readOne("12345"))
        assertEquals(-12345L, reader.readOne("-12345"))
        assertEquals(0L, reader.readOne("0"))
        assertEquals(0.5, reader.readOne("0.5"))
        assertEquals(-0.5, reader.readOne("-0.5"))
        assertEquals(0.0, reader.readOne("0.0"))
        assertEquals(1235.0, reader.readOne("1235.0"))
        assertEquals(1235.0, reader.readOne("1235."))
        assertEquals(-1235.0, reader.readOne("-1235."))
        assertEquals(.5, reader.readOne(".5"))
        assertEquals(-.5, reader.readOne("-.5"))
        assertEquals(Ratio.valueOf("-1", "2"), reader.readOne("#e#d-.5"))
        assertEquals(Ratio.valueOf("-1", "2"), reader.readOne("#E#d-.5"))
        assertEquals(+4.5, reader.readOne("#i#d+4.5"))
        assertEquals(4999999.5, reader.readOne("#i#d+4999999.5"))
        assertEquals(5L, reader.readOne("#e#b101"))
        assertEquals(5L, reader.readOne("#b#e101"))
        assertEquals(1L, reader.readOne("#b#e0001"))
        assertEquals(5L, reader.readOne("#E#b101"))
        assertEquals(5L, reader.readOne("#b#E101"))
        assertEquals(1L, reader.readOne("#b#E0001"))
        assertEquals(455L, reader.readOne("#o0707"))
        assertEquals(585L, reader.readOne("#o1111"))
        assertEquals(BigInteger("324518553658426726783156020576255"), reader.readOne("#xfffffffffffffffffffffffffff"))
        assertEquals(255.99609375, reader.readOne("#d255.99609375"))
        assertEquals(255.99609375, reader.readOne("#xff.ff"))
        assertEquals(171.67111108726925, reader.readOne("#xab.abcdefabcdef"))
        assertEquals(3.3125, reader.readOne("#b11.0101"))
        assertEquals(2730.661460876465, reader.readOne("#b101010101010.10101001010101011"))
        assertEquals(83.97128295898438, reader.readOne("#o123.76123"))
        assertEquals(2054353.1632647514, reader.readOne("#o7654321.1234567"))
        assertEquals(1500.0, reader.readOne("15##"))
        assertEquals(1500.0, reader.readOne("15##."))
        assertEquals(1500.0, reader.readOne("15##.#"))
        assertEquals(1500.0, reader.readOne("15##.####"))
        assertEquals(1500.0, reader.readOne("#i15##.####"))
        assertEquals(1500.0, reader.readOne("#i15##"))
        assertEquals(1500.0, reader.readOne("#I15##.####"))
        assertEquals(1500.0, reader.readOne("#I15##"))
        assertEquals(Ratio.valueOf("500", "1"), reader.readOne("#e5###/1#"))
        assertEquals(Ratio.valueOf("500", "1"), reader.readOne("#E5###/1#"))
        assertEquals(BigDecimal("500.0"), reader.readOne(" 5###/1#"))
        assertEquals(Ratio.valueOf("1500", "1"), reader.readOne("#e15##.####"))
        assertEquals(BigDecimal("0.75"), reader.readOne("#i3/4"))
        assertEquals(Ratio.valueOf("3", "4"), reader.readOne("#e3/4"))
        assertEquals(1500.0, reader.readOne("15e2"))
        assertEquals(15000.0, reader.readOne("15e3"))
        assertEquals(Ratio.valueOf("999999999999999999999999999999999999999999999999999999999999999999999999", "1000"),
                     reader.readOne("#e999999999999999999999999999999999999999999999999999999999999999999999.999"))
        assertEquals(BigDecimal("500.0"), reader.readOne("1/2e3"))
        assertEquals(BigDecimal("500.0"), reader.readOne("5/10e3"))
        assertEquals(Ratio.valueOf("-6000", "7"), reader.readOne("#e-12/14e3"))
        assertEquals(Ratio.valueOf("1000000", "1"), reader.readOne("#e+12/12e6"))
        assertEquals(BigDecimal("0.08333333333333333"), reader.readOne("1/12e0"))
        assertEquals(Ratio.valueOf("1", "12"), reader.readOne("#e+1/12e0"))

        val badNumbers = arrayOf("#o9999", "#df999", "#xz999", "#b2222", "#d+5+5", "#e##", "#e#e", "#e#I", "#ee##",
                                 "#e#i1", "#b#d#e12", "#b#d", "#i#o#I1", "#B#", "#B#B#B", "#ez#1", "#e_", "#D-", "#o++", "#o#b+1")
        for (bad in badNumbers) {
            try {
                reader.readOne(bad)
                fail()
            } catch (e: IllegalSyntaxException) {
                assertEquals("reader: bad number: " + bad, e.message)
            }
        }
    }

    @Test
    fun testReadComplex() {
        assertEquals(0L, reader.readOne("0+0i"))
        assertEquals(Complex(BigDecimal.ZERO, BigDecimal.ONE), reader.readOne("0+i"))
        assertEquals(Complex(BigDecimal.ZERO, BigDecimal.ONE), reader.readOne("-0+i"))
        assertEquals(Complex(BigDecimal.ZERO, BigDecimal.ONE), reader.readOne("+i"))
        assertEquals(Complex(BigDecimal.ZERO, BigDecimal(-1)), reader.readOne("-i"))
        assertEquals(Complex(BigDecimal.ONE, BigDecimal(2)), reader.readOne("1+2i"))
        assertEquals(Complex(BigDecimal.ONE, BigDecimal(-2)), reader.readOne("1-2i"))
        assertEquals(Complex(BigDecimal(-1), BigDecimal(2)), reader.readOne("-1+2i"))
        assertEquals(Complex(BigDecimal(-1), BigDecimal(-2)), reader.readOne("-1-2i"))
        assertEquals(Complex(BigDecimal.ONE, BigDecimal(2)), reader.readOne("#e1+2i"))
        assertEquals(Complex(BigDecimal("1.0"), BigDecimal("2.0")), reader.readOne("#i1+2i"))
        assertEquals(Complex(BigDecimal("5"), BigDecimal("29")), reader.readOne("#e#b101+11101i"))
        assertEquals(Complex(BigDecimal("5"), BigDecimal("29")), reader.readOne("#e#b101+11101i"))
        assertEquals(Complex(BigDecimal("255.0"), BigDecimal("2987.9375")), reader.readOne("#x#iFf+BaB.fI"))
        assertTrue(reader.readOne("1+2i+2i") is Symbol)
    }

    @Test
    fun testReadStrings() {
        assertEquals("1", reader.readOne("\"1\""))
        assertEquals("Lorem ipsum", reader.readOne("\"Lorem ipsum\""))
        assertEquals("Lorem \"ipsum\" ", reader.readOne("\"Lorem \\\"ipsum\\\" \""))
        assertEquals("", reader.readOne("\"\""))
        assertEquals("test \u0123", reader.readOne("\"test \\u123\""))
        assertEquals("test \\u", reader.readOne("\"test \\\\u\""))
        assertEquals("test \\U", reader.readOne("\"test \\\\U\""))
        assertEquals("test \\x", reader.readOne("\"test \\\\x\""))
        assertEquals("abc", reader.readOne("\"\\u61\\u62\\u63\""))
        assertEquals("abcd", reader.readOne("\"\\u61\\u62\\U63\\U64\""))
        assertEquals("జ్ఞ\u200Cా", reader.readOne("\"\\u0c1c\\u0c4d\\u0c1e\\u200c\\u0c3e\""))
        assertEquals("SS", reader.readOne("\"\\123\\123\""))
        try {
            reader.readOne("\"\\zzzz\"")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: unknown escape sequence \\z in string", e.message)
        }
        try {
            reader.readOne("\"test \\u\"")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: no hex digit following \\u in string", e.message)
        }
    }

    @Test
    fun testReadVector() {
        assertEquals(MutableVector(), reader.readOne("#()"))
        assertEquals(MutableVector(arrayOf(0L)), reader.readOne("[0]"))
        assertEquals(MutableVector(arrayOf(1L, 2L, 3L)), reader.readOne("[1 2 3]"))
        assertEquals(MutableVector(arrayOf(1L, "test", 3L)), reader.readOne("[1 \"test\" 3]"))
        assertEquals(MutableVector(arrayOf(1L, MutableVector(arrayOf(2L)), 3L)), reader.readOne("[1 [2] 3]"))
        assertEquals(MutableVector(), reader.readOne("[]"))
        assertEquals(MutableVector(arrayOf(0L)), reader.readOne("[0]"))
        assertEquals(MutableVector(arrayOf(1L, 2L, 3L)), reader.readOne("[1 2 3]"))
        assertEquals(MutableVector(arrayOf(1L, "test", 3L)), reader.readOne("[1 \"test\" 3]"))
        assertEquals(MutableVector(arrayOf(1L, MutableVector(arrayOf(2L)), 3L)), reader.readOne("[1 [2] 3]"))
        try {
            reader.readOne("#(1 . 2)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: illegal use of '.'", e.message)
        }
        try {
            reader.readOne("#(1 2 3 4 5 . 6)")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: illegal use of '.'", e.message)
        }
    }

    @Test
    fun testReadDot() {
        // "'(. 1)", "'(. 1 2)",
        val illegals = arrayOf("#(1 . 2)", "'(0 . 1 2 . 4)", "'(0 1 . 2 4)")
        for (illegal in illegals) {
            try {
                reader.readOne(illegal)
                fail()
            } catch (e: IllegalSyntaxException) {
                assertEquals("reader: illegal use of '.'", e.message)
            }
        }
    }

    @Test
    fun testReadList() {
        assertEquals(emptyList<Nothing>(), reader.readOne("()"))
        assertEquals(listOf(0L), reader.readOne("(0)"))
        assertEquals(listOf(1L, 2L, 3L), reader.readOne("(1 2 3)"))
        assertEquals(listOf<Any?>(1L, "test", 3L), reader.readOne("(1 \"test\" 3)"))
        assertEquals(listOf(1L, MutableVector(arrayOf(2L)), 3L), reader.readOne("(1 [2] 3)"))
        assertEquals(listOf(1L, listOf(2L), 3L), reader.readOne("(1 (2) 3)"))
        try {
            reader.readOne(")")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: unexpected list terminator: )", e.message)
        }
        try {
            reader.readOne("}")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: unexpected terminator: }", e.message)
        }
        try {
            reader.readOne("]")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: unexpected vector terminator: ]", e.message)
        }
    }

    @Test
    fun testReadWhitespace() {
        assertEquals(Unit, reader.readOne(""))
        assertEquals(Unit, reader.readOne("\t"))
        assertEquals(Unit, reader.readOne("\n\r"))
        assertEquals(listOf(s("+"), 1L, 2L, 3L), reader.readOne("(+ 1 2 \t \n     \r 3\t\n\r)"))
    }

    @Test
    fun testReadQuote() {
        assertEquals(listOf(Quote.symbol, 1L), reader.readOne("'1"))
        assertEquals(listOf(Quote.symbol, listOf<Any?>(1L, "test")), reader.readOne("'(1 \"test\")"))
        assertEquals(listOf(Quote.symbol, listOf(s(Quote.toString()), 1L)), reader.readOne("''1"))
    }

    @Test
    fun testReadQuasiquote() {
        assertEquals(listOf(Quasiquote.symbol, 1L), reader.readOne("`1"))
        assertEquals(listOf(Quasiquote.symbol, listOf<Any?>(1L, "test")), reader.readOne("`(1 \"test\")"))
        assertEquals(listOf(Quasiquote.symbol, listOf(s(Quote.toString()), 1L)), reader.readOne("`'1"))
    }

    @Test
    fun testReadComment() {
        assertEquals(Unit, reader.readOne(";test"))
        assertEquals(Unit, reader.readOne(";test\ttest"))
        assertEquals(Unit, reader.readOne(";test\t\ntest"))
        assertEquals(reader.readOne("1"), reader.readOne("1; wefewffewfwfwe \t \t few fwe f wf wfw ;w effw efw e "))
        assertEquals(reader.readOne("1"), reader.readOne("1 ; test"))
        assertEquals(reader.readOne("'(1 \"a\" 5)"), reader.readOne("'(1 \"a\" 5) ; test"))
    }

    @Test
    fun testReadCharacter() {
        assertEquals('A', reader.readOne("#\\A"))
        assertEquals('z', reader.readOne("#\\z"))
        assertEquals('\u0005', reader.readOne("#\\u0005"))
        assertEquals(' ', reader.readOne("#\\space"))
        assertEquals('\n', reader.readOne("#\\newline"))
        assertEquals('\u000B', reader.readOne("#\\u000b"))
        assertEquals('\u0009', reader.readOne("#\\u0009"))
        assertEquals('\u0000', reader.readOne("#\\u0000"))
        assertEquals('8', reader.readOne("#\\8"))
        assertEquals('\u0003', reader.readOne("#\\u0003"))
        assertEquals('\u0007', reader.readOne("#\\alarm"))
        assertEquals('\u0000', reader.readOne("#\\nul"))
        assertEquals('\u0000', reader.readOne("#\\null"))
        assertEquals(Character.MIN_VALUE, reader.readOne("#\\null"))
        assertEquals(Character.MIN_VALUE, reader.readOne("#\\u00000"))
        assertEquals(Character.MIN_VALUE, reader.readOne("#\\u0"))
        assertEquals('\r', reader.readOne("#\\ud"))
        assertEquals('\u000b', reader.readOne("#\\ub"))
        assertEquals('\u000b', reader.readOne("#\\u000b"))
        assertEquals('\u000b', reader.readOne("#\\u000000b"))
        assertEquals('u', reader.readOne("#\\u"))
        assertEquals('U', reader.readOne("#\\U"))
        assertEquals('x', reader.readOne("#\\x"))
        assertEquals('X', reader.readOne("#\\X"))
        assertEquals('S', reader.readOne("#\\123"))
        assertEquals('ÿ', reader.readOne("#\\377"))
        assertEquals('జ', reader.readOne("#\\u0c1c"))
        try {
            reader.readOne("#\\378")
        } catch (e: IllegalSyntaxException) {
            // success
        }
        try {
            reader.readOne("#\\999")
        } catch (e: IllegalSyntaxException) {
            // success
        }
    }

    @Test
    fun testReadBoolean() {
        assertEquals(true,  reader.readOne("#t"))
        assertEquals(false, reader.readOne("#f"))
        assertEquals(true,  reader.readOne("#T"))
        assertEquals(false, reader.readOne("#F"))
        assertEquals(true,  reader.readOne("true"))
        assertEquals(false, reader.readOne("false"))
    }

    @Test
    fun testReadIdentifier() {
        val ids = arrayOf("test", "lambda", "list->vector", "+", "<=?", "the-word-recursion-has-many-meanings",
                          "soup", "a", "ab", "+5+5", "1/1/1", "---", "123_", "....", "&", "$", "~", "//", "ab-3i",
                          "3-ai", "1/1/-2i")
        Arrays.stream(ids).forEach { assertEquals(s(it), reader.readOne(it)) }
    }

    @Test
    fun testScientificNotation() {
        assertEquals(Ratio.valueOf("23", "1"), reader.readOne("#e2.3e1"))
        assertEquals(230L,    reader.readOne("#e23e1"))
        assertEquals(2.3e-5,  reader.readOne("#i2.3e-5"))
        assertEquals(2.3e-51, reader.readOne("#i2.3e-51"))
        assertEquals(2.3e-5,  reader.readOne("#I2.3e-5"))
        assertEquals(2.3e-51, reader.readOne("#I2.3e-51"))
        assertEquals(92160.0, reader.readOne("#b101101e1011"))
        assertEquals(4484907929698304.0, reader.readOne("#xfefsa"))
        assertEquals(-234.0, reader.readOne("-234e0"))
        assertEquals(Double.POSITIVE_INFINITY, reader.readOne("1e999999999"))
        assertEquals(Double.NEGATIVE_INFINITY, reader.readOne("-1e999999999"))
        assertEquals(0.0, reader.readOne("1e-1000"))
        assertEquals(-0.0, reader.readOne("-1e-1000"))
        val badExps = arrayOf("#e23e1.3", "#e34e1.")
        for (bad in badExps) {
            try {
                reader.readOne(bad)
                fail()
            } catch (e: IllegalSyntaxException) {
                // expected
            }
        }
    }

    @Test
    fun testEscapeSequences() {
        val escape = "\"\\\\\t\b\n\r\\\"\""
        val expected = "\\\t\b\n\r\""
        assertEquals(expected, reader.readOne(escape))
        try {
            reader.readOne("\"\\u\"")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: no hex digit following \\u in string", e.message)
        }
        try {
            reader.readOne("\"\\x\"")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: no hex digit following \\x in string", e.message)
        }
    }

    @Test
    fun testReadBadSyntax() {
        val badSyntax = arrayOf("#", "##", "###")
        for (bad in badSyntax) {
            try {
                reader.readOne(bad)
                fail()
            } catch (e: IllegalSyntaxException) {
                assertEquals("reader: bad syntax: $bad", e.message)
            }
        }
    }

    @Test
    fun testReadUnknownNamedCharacters() {
        try {
            reader.readOne("#\\qwerty")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("reader: bad character constant: #\\qwerty", e.message)
        }
        val nohex = arrayOf("ui", "unknown", "uu")
        for (n in nohex) {
            try {
                reader.readOne("#\\$n")
                fail()
            } catch (e: IllegalSyntaxException) {
                assertEquals("reader: no hex digit following \\u in string", e.message)
            }
        }
    }

    @Test
    fun testReadHashmapLiteral() {
        assertTrue(reader.readOne("{}") is Map<*, *>)
        assertTrue(reader.readOne("{   }") is Map<*, *>)
        assertTrue(reader.readOne("{ ,, , , ,  }") is Map<*, *>)
        assertTrue(reader.readOne("  {    }  ") is Map<*, *>)
        assertTrue(reader.readOne("  {  1 2  }  ") is Map<*, *>)
        assertTrue(reader.readOne("  {  1 2,,,,  }  ") is Map<*, *>)
        assertTrue(reader.readOne("  {  1 2,3 4,,,  }  ") is Map<*, *>)
        assertTrue(reader.readOne("  {  1 2,3 4,  , , ,,,4 5  }  ") is Map<*, *>)
        assertEquals(4, (reader.readOne("  {  1 2,3 4, 5 6    ,  7     8  }  ") as Map<*, *>).size.toLong())
        try {
            reader.readOne("{1}")
        } catch (e: IllegalSyntaxException) {
            // expected
        }
        try {
            reader.readOne("{1 2 3}")
        } catch (e: IllegalSyntaxException) {
            // expected
        }
    }

    @Test
    fun testReadSetLiteral() {
        assertTrue(reader.readOne("#{}") is Set<*>)
        assertTrue(reader.readOne("#{   }") is Set<*>)
        assertTrue(reader.readOne("#{      }") is Set<*>)
        assertTrue(reader.readOne("  #{    }  ") is Set<*>)
        assertTrue(reader.readOne("  #{  1 2  }  ") is Set<*>)
        assertTrue(reader.readOne("  #{  1 2  }  ") is Set<*>)
        assertTrue(reader.readOne("  #{  1 2 3 4     }  ") is Set<*>)
        assertTrue(reader.readOne("  #{  1 2 3          4 5  }  ") is Set<*>)
        try {
            reader.readOne("  #{  1 2 3   4      4 5  }  ")
        } catch (e: IllegalArgumentException) {
            // expected
        }
        try {
            reader.readOne("  #{  1 1}  ")
        } catch (e: IllegalArgumentException) {
            // expected
        }
    }

    @Test
    fun testReadRegexPattern() {
        assertTrue(reader.readOne("#\"\"") is Pattern)
        assertTrue(reader.readOne("#\".*\"") is Pattern)
        assertTrue(reader.readOne("#\"[abcdef]\"") is Pattern)
        assertTrue(reader.readOne("#\"(a|b|c|d)\"") is Pattern)
    }

    @Test
    fun testReadKeywords() {
        assertEquals(Keyword.intern("a"), reader.readOne(":a"))
        assertEquals(Keyword.intern("test"), reader.readOne(":test"))
        assertEquals(Keyword.intern("_"), reader.readOne(":_"))
        try {
            reader.readOne(":")
            fail()
        } catch (e: IllegalSyntaxException) {
            // success
        }
    }

    @Test
    fun testReadThresholds() {
        assertEquals(Long.MAX_VALUE, reader.readOne(Long.MAX_VALUE.toString()))
        assertEquals(Long.MAX_VALUE, reader.readOne("+" + Long.MAX_VALUE.toString()))
        assertEquals(Long.MIN_VALUE, reader.readOne(Long.MIN_VALUE.toString()))

        val biMax = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE)
        val biMin = BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.ONE)
        assertEquals(biMax, reader.readOne(biMax.toString()))
        assertEquals(biMax, reader.readOne("+" + biMax.toString()))
        assertEquals(biMin, reader.readOne(biMin.toString()))

        assertEquals(Double::class.javaObjectType, reader.readOne(" 123456789012345.0")!!::class.java)
        assertEquals(Double::class.javaObjectType, reader.readOne("+123456789012345.0")!!::class.java)
        assertEquals(Double::class.javaObjectType, reader.readOne("-123456789012345.0")!!::class.java)

        assertEquals(BigDecimal::class.javaObjectType, reader.readOne(" 1234567890123456.0")!!::class.java)
        assertEquals(BigDecimal::class.javaObjectType, reader.readOne("+1234567890123456.0")!!::class.java)
        assertEquals(BigDecimal::class.javaObjectType, reader.readOne("-1234567890123456.0")!!::class.java)
    }

    @Test
    fun testReadSyntax() {
        assertTrue(reader.readOne("#'1") is Syntax)
        assertEquals(1L, (reader.readOne("#'1") as Syntax).template)
    }
}
