package unittests

import core.exceptions.WrongTypeException
import org.junit.Test

import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class CharacterTest : AbstractTest() {

    @Test
    fun testEvalIsChar() {
        assertEquals(true,  eval("(char? #\\A)", env))
        assertEquals(true,  eval("(char? #\\u0000)", env))
        assertEquals(true,  eval("(char? #\\u0300)", env))
        assertEquals(false, eval("(char? \"A\")", env))
    }

    @Test
    fun testEvalCharEq() {
        assertEquals(true,  eval("(char=? #\\A #\\A)", env))
        assertEquals(false, eval("(char=? #\\B #\\A)", env))
        assertEquals(true,  eval("(char=? #\\newline #\\newline)", env))
    }

    @Test
    fun testEvalCharEqCi() {
        assertEquals(true,  eval("(char-ci=? #\\Z #\\z)", env))
        assertEquals(false, eval("(char-ci=? #\\b #\\A)", env))
    }

    @Test
    fun testEvalCharNumeric() {
        assertEquals(false, eval("(char-numeric? #\\u0001)", env))
        assertEquals(false, eval("(char-numeric? #\\u0000)", env))
        assertEquals(true,  eval("(char-numeric? #\\9)", env))
        assertEquals(false, eval("(char-numeric? #\\b)", env))
        assertEquals(false, eval("(char-numeric? #\\.)", env))
        try {
            eval("(char-numeric? 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-numeric?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharWhitespace() {
        assertEquals(false, eval("(char-whitespace? #\\u0001)", env))
        assertEquals(false, eval("(char-whitespace? #\\u0000)", env))
        assertEquals(false, eval("(char-whitespace? #\\9)", env))
        assertEquals(false, eval("(char-whitespace? #\\b)", env))
        assertEquals(false, eval("(char-whitespace? #\\.)", env))
        assertEquals(false, eval("(char-whitespace? #\\backspace)", env))
        assertEquals(true,  eval("(char-whitespace? #\\newline)", env))
        assertEquals(true,  eval("(char-whitespace? #\\tab)", env))
        assertEquals(true,  eval("(char-whitespace? #\\vtab)", env))
        assertEquals(true,  eval("(char-whitespace? #\\return)", env))
        assertEquals(true,  eval("(char-whitespace? #\\space)", env))
        assertEquals(true,  eval("(char-whitespace? #\\linefeed)", env))
        try {
            eval("(char-whitespace? 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-whitespace?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharAlphabetic() {
        assertEquals(false, eval("(char-alphabetic? #\\u0001)", env))
        assertEquals(false, eval("(char-alphabetic? #\\u0000)", env))
        assertEquals(false, eval("(char-alphabetic? #\\9)", env))
        assertEquals(false, eval("(char-alphabetic? #\\.)", env))
        assertEquals(false, eval("(char-alphabetic? #\\backspace)", env))
        assertEquals(false, eval("(char-alphabetic? #\\newline)", env))
        assertEquals(false, eval("(char-alphabetic? #\\tab)", env))
        assertEquals(false, eval("(char-alphabetic? #\\vtab)", env))
        assertEquals(false, eval("(char-alphabetic? #\\return)", env))
        assertEquals(false, eval("(char-alphabetic? #\\space)", env))
        assertEquals(false, eval("(char-alphabetic? #\\linefeed)", env))

        assertEquals(true, eval("(char-alphabetic? #\\b)", env))
        assertEquals(true, eval("(char-alphabetic? #\\Z)", env))
        assertEquals(true, eval("(char-alphabetic? #\\g)", env))
        assertEquals(true, eval("(char-alphabetic? #\\I)", env))
        try {
            eval("(char-alphabetic? 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-alphabetic?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharUpperCase() {
        assertEquals(false, eval("(char-upper-case? #\\u0001)", env))
        assertEquals(false, eval("(char-upper-case? #\\u0000)", env))
        assertEquals(false, eval("(char-upper-case? #\\9)", env))
        assertEquals(false, eval("(char-upper-case? #\\b)", env))
        assertEquals(false, eval("(char-upper-case? #\\.)", env))
        assertEquals(false, eval("(char-upper-case? #\\a)", env))
        assertEquals(false, eval("(char-upper-case? #\\z)", env))
        assertEquals(false, eval("(char-upper-case? #\\i)", env))
        assertEquals(false, eval("(char-upper-case? #\\h)", env))

        assertEquals(true, eval("(char-upper-case? #\\A)", env))
        assertEquals(true, eval("(char-upper-case? #\\Z)", env))
        assertEquals(true, eval("(char-upper-case? #\\I)", env))
        assertEquals(true, eval("(char-upper-case? #\\H)", env))
        try {
            eval("(char-upper-case? 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-upper-case?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharLowerCase() {
        assertEquals(true, eval("(char-lower-case? #\\b)", env))
        assertEquals(true, eval("(char-lower-case? #\\a)", env))
        assertEquals(true, eval("(char-lower-case? #\\z)", env))
        assertEquals(true, eval("(char-lower-case? #\\i)", env))
        assertEquals(true, eval("(char-lower-case? #\\h)", env))

        assertEquals(false, eval("(char-lower-case? #\\A)", env))
        assertEquals(false, eval("(char-lower-case? #\\Z)", env))
        assertEquals(false, eval("(char-lower-case? #\\I)", env))
        assertEquals(false, eval("(char-lower-case? #\\H)", env))
        assertEquals(false, eval("(char-lower-case? #\\u0001)", env))
        assertEquals(false, eval("(char-lower-case? #\\u0000)", env))
        assertEquals(false, eval("(char-lower-case? #\\9)", env))
        assertEquals(false, eval("(char-lower-case? #\\.)", env))
        try {
            eval("(char-lower-case? 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-lower-case?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharToInteger() {
        assertEquals(1L, eval("(char->integer #\\u0001)", env))
        assertEquals(0L, eval("(char->integer #\\u0000)", env))
        assertEquals(57L, eval("(char->integer #\\9)", env))
        assertEquals(98L, eval("(char->integer #\\b)", env))
        assertEquals(46L, eval("(char->integer #\\.)", env))
        try {
            eval("(char->integer 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char->integer: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalIntegerToChar() {
        assertEquals('\u0001', eval("(integer->char (char->integer #\\u0001))", env))
        assertEquals('\u0000', eval("(integer->char (char->integer #\\u0000))", env))
        assertEquals('9',      eval("(integer->char (char->integer #\\9))", env))
        assertEquals('b',      eval("(integer->char (char->integer #\\b))", env))
        assertEquals('.',      eval("(integer->char (char->integer #\\.))", env))
        try {
            eval("(integer->char #\\a)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("integer->char: type mismatch; (expected: Integer, given: #\\a)", e.message)
        }
    }

    @Test
    fun testEvalCharUpcase() {
        assertEquals('\u0001', eval("(char-upcase #\\u0001)", env))
        assertEquals('\u0000', eval("(char-upcase #\\u0000)", env))
        assertEquals('9',      eval("(char-upcase #\\9)", env))
        assertEquals('B',      eval("(char-upcase #\\b)", env))
        assertEquals('Z',      eval("(char-upcase #\\z)", env))
        assertEquals('.',      eval("(char-upcase #\\.)", env))
        try {
            eval("(char-upcase 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-upcase: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharDowncase() {
        assertEquals('\u0001', eval("(char-downcase #\\u0001)", env))
        assertEquals('\u0000', eval("(char-downcase #\\u0000)", env))
        assertEquals('9',      eval("(char-downcase #\\9)", env))
        assertEquals('b',      eval("(char-downcase #\\B)", env))
        assertEquals('z',      eval("(char-downcase #\\Z)", env))
        assertEquals('.',      eval("(char-downcase #\\.)", env))
        try {
            eval("(char-downcase 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-downcase: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testCharComparison() {
        assertEquals(true, eval("(char<? #\\a #\\b)", env))
        assertEquals(true, eval("(char<=? #\\a #\\a #\\b)", env))
        assertEquals(false, eval("(char<=? #\\a #\\A #\\b)", env))
        assertEquals(false, eval("(char>? #\\z #\\A #\\H #\\a #\\X)", env))
        assertEquals(true, eval("(char>=? #\\z #\\z #\\x)", env))
    }
}
