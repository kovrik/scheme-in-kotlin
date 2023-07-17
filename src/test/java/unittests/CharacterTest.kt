package unittests

import core.exceptions.WrongTypeException
import org.junit.Test

import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class CharacterTest : AbstractTest() {

    @Test
    fun testEvalIsChar() {
        assertEquals(true,  eval("(char? #\\A)"))
        assertEquals(true,  eval("(char? #\\u0000)"))
        assertEquals(true,  eval("(char? #\\u0300)"))
        assertEquals(false, eval("(char? \"A\")"))
    }

    @Test
    fun testEvalCharEq() {
        assertEquals(true,  eval("(char=? #\\A #\\A)"))
        assertEquals(false, eval("(char=? #\\B #\\A)"))
        assertEquals(true,  eval("(char=? #\\newline #\\newline)"))
    }

    @Test
    fun testEvalCharEqCi() {
        assertEquals(true,  eval("(char-ci=? #\\Z #\\z)"))
        assertEquals(false, eval("(char-ci=? #\\b #\\A)"))
    }

    @Test
    fun testEvalCharNumeric() {
        assertEquals(false, eval("(char-numeric? #\\u0001)"))
        assertEquals(false, eval("(char-numeric? #\\u0000)"))
        assertEquals(true,  eval("(char-numeric? #\\9)"))
        assertEquals(false, eval("(char-numeric? #\\b)"))
        assertEquals(false, eval("(char-numeric? #\\.)"))
        try {
            eval("(char-numeric? 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-numeric?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharWhitespace() {
        assertEquals(false, eval("(char-whitespace? #\\u0001)"))
        assertEquals(false, eval("(char-whitespace? #\\u0000)"))
        assertEquals(false, eval("(char-whitespace? #\\9)"))
        assertEquals(false, eval("(char-whitespace? #\\b)"))
        assertEquals(false, eval("(char-whitespace? #\\.)"))
        assertEquals(false, eval("(char-whitespace? #\\backspace)"))
        assertEquals(true,  eval("(char-whitespace? #\\newline)"))
        assertEquals(true,  eval("(char-whitespace? #\\tab)"))
        assertEquals(true,  eval("(char-whitespace? #\\vtab)"))
        assertEquals(true,  eval("(char-whitespace? #\\return)"))
        assertEquals(true,  eval("(char-whitespace? #\\space)"))
        assertEquals(true,  eval("(char-whitespace? #\\linefeed)"))
        try {
            eval("(char-whitespace? 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-whitespace?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharAlphabetic() {
        assertEquals(false, eval("(char-alphabetic? #\\u0001)"))
        assertEquals(false, eval("(char-alphabetic? #\\u0000)"))
        assertEquals(false, eval("(char-alphabetic? #\\9)"))
        assertEquals(false, eval("(char-alphabetic? #\\.)"))
        assertEquals(false, eval("(char-alphabetic? #\\backspace)"))
        assertEquals(false, eval("(char-alphabetic? #\\newline)"))
        assertEquals(false, eval("(char-alphabetic? #\\tab)"))
        assertEquals(false, eval("(char-alphabetic? #\\vtab)"))
        assertEquals(false, eval("(char-alphabetic? #\\return)"))
        assertEquals(false, eval("(char-alphabetic? #\\space)"))
        assertEquals(false, eval("(char-alphabetic? #\\linefeed)"))

        assertEquals(true, eval("(char-alphabetic? #\\b)"))
        assertEquals(true, eval("(char-alphabetic? #\\Z)"))
        assertEquals(true, eval("(char-alphabetic? #\\g)"))
        assertEquals(true, eval("(char-alphabetic? #\\I)"))
        try {
            eval("(char-alphabetic? 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-alphabetic?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharUpperCase() {
        assertEquals(false, eval("(char-upper-case? #\\u0001)"))
        assertEquals(false, eval("(char-upper-case? #\\u0000)"))
        assertEquals(false, eval("(char-upper-case? #\\9)"))
        assertEquals(false, eval("(char-upper-case? #\\b)"))
        assertEquals(false, eval("(char-upper-case? #\\.)"))
        assertEquals(false, eval("(char-upper-case? #\\a)"))
        assertEquals(false, eval("(char-upper-case? #\\z)"))
        assertEquals(false, eval("(char-upper-case? #\\i)"))
        assertEquals(false, eval("(char-upper-case? #\\h)"))

        assertEquals(true, eval("(char-upper-case? #\\A)"))
        assertEquals(true, eval("(char-upper-case? #\\Z)"))
        assertEquals(true, eval("(char-upper-case? #\\I)"))
        assertEquals(true, eval("(char-upper-case? #\\H)"))
        try {
            eval("(char-upper-case? 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-upper-case?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharLowerCase() {
        assertEquals(true, eval("(char-lower-case? #\\b)"))
        assertEquals(true, eval("(char-lower-case? #\\a)"))
        assertEquals(true, eval("(char-lower-case? #\\z)"))
        assertEquals(true, eval("(char-lower-case? #\\i)"))
        assertEquals(true, eval("(char-lower-case? #\\h)"))

        assertEquals(false, eval("(char-lower-case? #\\A)"))
        assertEquals(false, eval("(char-lower-case? #\\Z)"))
        assertEquals(false, eval("(char-lower-case? #\\I)"))
        assertEquals(false, eval("(char-lower-case? #\\H)"))
        assertEquals(false, eval("(char-lower-case? #\\u0001)"))
        assertEquals(false, eval("(char-lower-case? #\\u0000)"))
        assertEquals(false, eval("(char-lower-case? #\\9)"))
        assertEquals(false, eval("(char-lower-case? #\\.)"))
        try {
            eval("(char-lower-case? 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-lower-case?: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharToInteger() {
        assertEquals(1L, eval("(char->integer #\\u0001)"))
        assertEquals(0L, eval("(char->integer #\\u0000)"))
        assertEquals(57L, eval("(char->integer #\\9)"))
        assertEquals(98L, eval("(char->integer #\\b)"))
        assertEquals(46L, eval("(char->integer #\\.)"))
        try {
            eval("(char->integer 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char->integer: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalIntegerToChar() {
        assertEquals('\u0001', eval("(integer->char (char->integer #\\u0001))"))
        assertEquals('\u0000', eval("(integer->char (char->integer #\\u0000))"))
        assertEquals('9',      eval("(integer->char (char->integer #\\9))"))
        assertEquals('b',      eval("(integer->char (char->integer #\\b))"))
        assertEquals('.',      eval("(integer->char (char->integer #\\.))"))
        try {
            eval("(integer->char #\\a)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("integer->char: type mismatch; (expected: Integer, given: #\\a)", e.message)
        }
    }

    @Test
    fun testEvalCharUpcase() {
        assertEquals('\u0001', eval("(char-upcase #\\u0001)"))
        assertEquals('\u0000', eval("(char-upcase #\\u0000)"))
        assertEquals('9',      eval("(char-upcase #\\9)"))
        assertEquals('B',      eval("(char-upcase #\\b)"))
        assertEquals('Z',      eval("(char-upcase #\\z)"))
        assertEquals('.',      eval("(char-upcase #\\.)"))
        try {
            eval("(char-upcase 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-upcase: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalCharDowncase() {
        assertEquals('\u0001', eval("(char-downcase #\\u0001)"))
        assertEquals('\u0000', eval("(char-downcase #\\u0000)"))
        assertEquals('9',      eval("(char-downcase #\\9)"))
        assertEquals('b',      eval("(char-downcase #\\B)"))
        assertEquals('z',      eval("(char-downcase #\\Z)"))
        assertEquals('.',      eval("(char-downcase #\\.)"))
        try {
            eval("(char-downcase 1)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("char-downcase: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testCharComparison() {
        assertEquals(true, eval("(char<? #\\a #\\b)"))
        assertEquals(true, eval("(char<=? #\\a #\\a #\\b)"))
        assertEquals(false, eval("(char<=? #\\a #\\A #\\b)"))
        assertEquals(false, eval("(char>? #\\z #\\A #\\H #\\a #\\X)"))
        assertEquals(true, eval("(char>=? #\\z #\\z #\\x)"))
    }
}
