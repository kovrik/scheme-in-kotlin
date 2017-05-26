package unittests

import org.junit.Test

import java.lang.Boolean.TRUE
import org.junit.Assert.assertEquals

class TCOTest : AbstractTest() {

    companion object {
        private val ITERS = 100000L
    }

    @Test
    fun testIfTCO() {
        val recur = "(define (recur n)" +
                "  (if (zero? n)" +
                "      \"DONE\"" +
                "      (recur (- n 1))))"
        eval(recur, env)
        assertEquals("DONE", eval("(recur $ITERS)", env))
    }

    @Test
    fun testOrTCO() {
        val recursive = "(define (recOr n) (or (zero? n) (recOr (- n 1))))"
        eval(recursive, env)
        assertEquals(TRUE, eval("(recOr $ITERS)", env))
    }

    @Test
    fun testDefineAndBeginTCO() {
        val recursive = "(define (foo n) (if (= n $ITERS) n (foo (+ n 1)))"
        eval(recursive, env)
        assertEquals(ITERS, eval("(foo 5)", env))
    }
}
