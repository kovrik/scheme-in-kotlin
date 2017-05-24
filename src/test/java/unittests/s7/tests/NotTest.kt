package unittests.s7.tests

import org.junit.Test
import unittests.AbstractTest

import java.lang.Boolean.FALSE
import java.lang.Boolean.TRUE
import org.junit.Assert.assertEquals

class NotTest : AbstractTest() {

    @Test
    fun testNot() {
        assertEquals(TRUE, eval("(not #f)", env))
        assertEquals(TRUE, eval("(not (not #t))", env))
        val falses = arrayOf(
                "(not #t)", "(not 0)", "(not 1)", "(not '())", "(not 't)", "(not (list))", "(not (list 3))",
                "(not 'nil)", "(not not)", "(not \"\")", "(not 'lambda)", "(not 'quote)", "(not 'and)", "(not 'case)")
        assertAllEqual(FALSE, falses, env)
    }
}
