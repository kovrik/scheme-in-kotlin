package unittests.s7.tests

import org.junit.Test
import unittests.AbstractTest

import org.junit.Assert.assertEquals

class NotTest : AbstractTest() {

    @Test
    fun testNot() {
        assertEquals(true, eval("(not #f)"))
        assertEquals(true, eval("(not (not #t))"))
        val falses = arrayOf(
                "(not #t)", "(not 0)", "(not 1)", "(not '())", "(not 't)", "(not (list))", "(not (list 3))",
                "(not not)", "(not \"\")", "(not 'lambda)", "(not 'quote)", "(not 'and)", "(not 'case)")
        assertAllEqual(false, falses)
    }
}
