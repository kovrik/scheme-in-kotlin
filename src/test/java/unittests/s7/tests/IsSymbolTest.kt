package unittests.s7.tests

import core.exceptions.IllegalSyntaxException
import org.junit.Test
import unittests.AbstractTest

import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class IsSymbolTest : AbstractTest() {

    @Test
    fun testIsSymbol() {
        val trues = arrayOf("(symbol? 't)  ", "(symbol? 'foo)", "(symbol? (car '(a b)))", "(symbol? 'nil)",
                "(symbol? 'car)", "(symbol? '_)", "(symbol? '|)", "(symbol? '|')",
                "(symbol? 'sym0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789)",
                "(symbol? (vector-ref '[1 a 34] 1))", "(symbol? (string->symbol \"if\"))", "(symbol? 'quote)",
                "(symbol? 'begin)", "(symbol? 'if)", "(if (symbol? '1+) (symbol? '0e) #t)", "(if (symbol? '1+) (symbol? '0000eeesve) #t)")
        assertAllEqual(true, trues, env)

        val falses = arrayOf(
                "(symbol? '(AB\\c () xyz))", "(symbol? #b1)", "(symbol? car)", "(symbol? '#f)", "(symbol? #())",
                "(symbol? '())", "(symbol? #())", "(symbol? #f)", "(symbol? \"t\") ", "(symbol? '(t))",
                "(symbol? #t)  ", "(symbol? 4)   ")
        assertAllEqual(false, falses, env)

        try {
            eval("(if (symbol? '1+) (symbol? '#xff0000eeesve) #t)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("read: bad exponent: ff0000eeesve", e.message)
        }
    }
}
