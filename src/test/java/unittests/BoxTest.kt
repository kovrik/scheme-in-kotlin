package unittests

import core.exceptions.WrongTypeException
import core.scm.Box
import org.junit.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class BoxTest : AbstractTest() {

    @Test
    fun testEvalBox() {
        assertEquals(Box::class, eval("(box  nil)")!!::class)
        assertEquals(Box::class, eval("(atom nil)")!!::class)
    }

    @Test
    fun testEvalUnbox() {
        assertEquals(6L,   eval("(unbox (box (+ 1 2 3)))"))
        assertEquals(6L,   eval("@(box (+ 1 2 3))"))
        assertEquals(null, eval("@(box nil)"))
        try {
            eval("(unbox 1)")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testEvalSetBox() {
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (set-box! b 9) @b)"))
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (set-box! b (* 3 3)) @b)"))
    }

    @Test
    fun testEvalReset() {
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (reset! b 9))"))
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (reset! b (* 3 3)))"))
    }

    @Test
    fun testEvalBoxCas() {
        assertEquals(true,  eval("(let ((b (box (+ 1 2 3)))) (box-cas! b 6 9))"))
        assertEquals(false, eval("(let ((b (box (+ 1 2 3)))) (box-cas! b (* 3 3) 12))"))
    }

    @Test
    fun testEvalSwap() {
        assertEquals(7L,  eval("(let ((b (box (+ 1 2 3)))) (swap! b inc))"))
        assertEquals(6L,  eval("(let ((b (box (+ 1 2 3)))) (swap! b identity))"))
        assertEquals(7L,  eval("(let ((b (box (+ 1 2 3)))) (swap! b (lambda (n) (+ n 1)) ))"))
        assertEquals(20L, eval("(let ((b (box (+ 1 2 3)))) (swap! b (lambda (n1 n2 n3) (+ n1 n2 n3)) 7 7))"))
        assertEquals(20L, eval("(let ((b (box (+ 1 2 3)))) (swap! b (lambda (n . rest) (apply + n rest)) 7 7))"))
    }

    @Test
    fun testEvalIsBox() {
        assertEquals(true,  eval("(box?  (box  nil))"))
        assertEquals(true,  eval("(atom? (box  nil))"))
        assertEquals(true,  eval("(box?  (atom nil))"))
        assertEquals(true,  eval("(atom? (atom nil))"))
        assertEquals(false, eval("(box?  1)"))
        assertEquals(false, eval("(atom? 1)"))
        assertEquals(false, eval("(box?  (delay 1))"))
        assertEquals(false, eval("(atom? (promise))"))
    }
}
