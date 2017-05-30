package unittests

import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test

class RatioTest : AbstractTest() {

    @Test
    fun testZero() {
        assertEquals(0L, eval("0/1", env))
        assertEquals(0L, eval("0000/1111", env))
        assertEquals(0L, eval("-0000/1111", env))
        try {
            eval("1/0", env)
            fail()
        } catch (e: ArithmeticException) {
            assertEquals("Division by zero", e.message)
        }
    }

    @Test
    fun testOne() {
        assertEquals(1L, eval("1/1", env))
        assertEquals(1L, eval("1111/1111", env))
        assertEquals(1L, eval("12345/12345", env))
    }
}
