package unittests

import core.exceptions.IllegalSyntaxException
import core.exceptions.ReentrantDelayException
import core.exceptions.WrongTypeException
import core.scm.Keyword
import org.junit.Assert.*
import org.junit.Test

class DelayedTest : AbstractTest() {

    @Test
    fun testEvalDelay() {
        assertEquals(true,  eval("(promise?   (delay (* (+ 2 3) 4))))"))
        assertEquals(false, eval("(procedure? (delay (* (+ 2 3) 4))))"))
    }

    @Test
    fun testEvalDelayed() {
        try {
            eval("((delay (* (+ 2 3) 4)))")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertTrue(e.message!!.contains("wrong type to apply"))
        }
    }

    @Test
    fun testEvalForce() {
        assertEquals(true, eval("(force (delay (= (+ 1 2) 3)))"))
        assertEquals(5L, eval("(force (delay 1 2 3 4 5))"))
    }

    @Test
    fun testReentrantDelay() {
        /* Check that re-entrant promises are not allowed
         * See http://lambda-the-ultimate.org/node/4686A
         */
        eval("(define x 0)")
        val conundrum = "(define p" +
                        "  (delay" +
                        "    (if (= x 5)" +
                        "      x" +
                        "      (begin" +
                        "        (set! x (+ x 1))" +
                        "        (force p)" +
                        "        (set! x (+ x 1))" +
                        "        x))))"
        eval(conundrum)
        try {
            eval("(force p)")
            fail()
        } catch (e: ReentrantDelayException) {
            assertTrue(e.message!!.startsWith("re-entrant delay:"))
        }
    }

    @Test
    fun testPromise() {
        assertEquals(false, eval("(let ((p (promise))) (realized? p))"))
        assertEquals(false, eval("(let ((p (promise))) (future?   p))"))
        assertEquals(false, eval("(let ((p (promise))) (delay?    p))"))
        assertEquals(true,  eval("(let ((p (promise))) (promise?  p))"))
        assertEquals(true,  eval("(let ((p (promise))) (deliver p 1) (realized? p))"))
        assertEquals(12L,   eval("(let ((p (promise))) (deliver p (+ 1 2 3)) (+ @p (deref p)))"))
        assertEquals(Keyword.intern("timeout"), eval("(deref (promise) 1 :timeout)"))
    }

    @Test
    fun testDelay() {
        assertEquals(false, eval("(let ((d (delay (+ 1 2 3)))) (realized? d))"))
        assertEquals(false, eval("(let ((d (delay (+ 1 2 3)))) (future?   d))"))
        assertEquals(true,  eval("(let ((d (delay (+ 1 2 3)))) (delay?    d))"))
        assertEquals(true,  eval("(let ((d (delay (+ 1 2 3)))) (promise?  d))"))
        assertEquals(true,  eval("(let ((d (delay (+ 1 2 3)))) @d (realized? d))"))
        assertEquals(6L,    eval("(let ((d (delay (+ 1 2 3)))) @d)"))
    }

    @Test
    fun testFuture() {
        assertEquals(true,  eval("(let ((f (future (+ 1 2 3)))) (future?   f))"))
        assertEquals(false, eval("(let ((f (future (+ 1 2 3)))) (delay?    f))"))
        assertEquals(false, eval("(let ((f (future (+ 1 2 3)))) (promise?  f))"))
        assertEquals(6L,    eval("(let ((f (future (+ 1 2 3)))) @f)"))
        assertEquals(true,  eval("(let ((f (future (+ 1 2 3)))) @f (future-done? f))"))
        assertEquals(true,  eval("(let ((f (future (sleep 5000)))) (future-cancel f) (future-cancelled? f))"));
        assertEquals(Keyword.intern("timeout"), eval("(deref (future (sleep 1000000)) 1 :timeout)"))
        try {
            eval("(realized? 123)")
            fail()
        } catch (e: WrongTypeException) {
            // success
        }
    }

    @Test
    fun testIsLazySeqRealized() {
        assertEquals(false, eval("(realized? (lazy-seq (range 5)))"))
        assertEquals(false, eval("(let ((s (lazy-seq (range)))) (take 1 s) (realized? s))"))
        assertEquals(true,  eval("(let ((s (lazy-seq (range)))) (str (take 1 s)) (realized? s))"))
    }
}
