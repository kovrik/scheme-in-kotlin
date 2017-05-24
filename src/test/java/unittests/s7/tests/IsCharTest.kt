package unittests.s7.tests

import core.exceptions.ArityException
import org.junit.Test
import unittests.AbstractTest

import java.lang.Boolean.FALSE
import java.lang.Boolean.TRUE
import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class IsCharTest : AbstractTest() {

    @Test
    fun testIsChar() {
        val trues = arrayOf("(char? #\\a)", "(char? #\\()", "(char? #\\space)", "(char? '#\\newline)", "(char? #\\u0001)",
                "(char? #\\$)", "(char? #\\.)", "(char? #\\\\)", "(char? #\\))", "(char? #\\%)", "(char? '#\\space)",
                "(char? '#\\ )", "(char? '#\\newline)", "(char? '#\\a)", "(char? '#\\8)", "(char? #\\-)", "(char? #\\n)",
                "(char? #\\()", "(char? #\\#)", "(char? #\\x)", "(char? #\\o)", "(char? #\\b)",
                "(char? (string-ref \"hi\" 0))", "(char? (string-ref (make-string 1) 0))", "(char? #\\\")", "(char? #\\\')",
                "(char? #\\`)", "(char? #\\@)", "(char? #\\u65)", "(char? #\\u000000000065)", "(char? #\\u0)", "(char=? #\\u000 #\\null)",
                "(char=? #\\u08 #\\u8)", "(char=? #\\u0e    #\\ue)", "(char=? #\\u00e   #\\ue)",
                "(char=? #\\u0000e #\\ue)", "(char=? #\\u00000000e #\\ue)", "(char? #\\uff)", "(char=? #\\u6a #\\j)",
                "(char=? #\\return #\\ud)", "(char=? #\\null #\\u0)", "(char? #\\return)", "(char? #\\null)",
                "(char? #\\nul)", "(char? #\\linefeed)", "(char? #\\tab)", "(char? #\\space)", "(char=? #\\null #\\nul)",
                "(char=? #\\newline #\\linefeed)", "(char? #\\backspace)", "(char? #\\escape)", "(char? #\\alarm)",
                "(char? #\\delete)")
        assertAllEqual(TRUE, trues, env)

        val falses = arrayOf(
                "(char=? #\\delete #\\backspace)", "(char? '1e311)", "(char? #e1)", "(char? #b101)", "(char? #o73)",
                "(char? #x73)", "(char? 'a)", "(char? 97)", "(char? \"a\")", "(char? 'begin)")
        assertAllEqual(FALSE, falses, env)

        try {
            eval("(char?) 'error)", env)
            fail()
        } catch (e: ArityException) {
            assertEquals("char?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.message)
        }

        try {
            eval("(char? #\\a #\\b)", env)
            fail()
        } catch (e: ArityException) {
            assertEquals("char?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 2)", e.message)
        }
    }
}
