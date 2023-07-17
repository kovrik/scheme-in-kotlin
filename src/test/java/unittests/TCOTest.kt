package unittests

import org.junit.Assert.assertEquals
import org.junit.Test

class TCOTest : AbstractTest() {

    companion object {
        private const val iterations = 10000L
    }

    @Test
    fun testIfTCO() {
        val recur = """(define (recur n)
                         (if (zero? n)
                           "DONE"
                           (recur (- n 1))))"""
        eval(recur)
        assertEquals("DONE", eval("(time (recur $iterations))"))
    }

    @Test
    fun testOrTCO() {
        val recursive = "(define (recOr n) (or (zero? n) (recOr (- n 1))))"
        eval(recursive)
        assertEquals(true, eval("(recOr $iterations)"))
    }

    @Test
    fun testDefineAndBeginTCO() {
        val recursive = "(define (foo n) (if (= n $iterations) n (foo (+ n 1)))"
        eval(recursive)
        assertEquals(iterations, eval("(foo 5)"))
    }
}
