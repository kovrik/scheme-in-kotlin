package unittests

import core.exceptions.WrongTypeException
import core.scm.Box
import org.junit.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class BoxTest : AbstractTest() {

    @Test
    fun testEvalBox() {
        assertEquals(Box::class, eval("(box  nil)", env)!!::class)
        assertEquals(Box::class, eval("(atom nil)", env)!!::class)
    }

    @Test
    fun testEvalUnbox() {
        assertEquals(6L,   eval("(unbox (box (+ 1 2 3)))", env))
        assertEquals(6L,   eval("@(box (+ 1 2 3))", env))
        assertEquals(null, eval("@(box nil)", env))
        try {
            eval("(unbox 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testEvalSetBox() {
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (set-box! b 9) @b)", env))
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (set-box! b (* 3 3)) @b)", env))
    }

    @Test
    fun testEvalReset() {
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (reset! b 9))", env))
        assertEquals(9L, eval("(let ((b (box (+ 1 2 3)))) (reset! b (* 3 3)))", env))
    }

    @Test
    fun testEvalBoxCas() {
        assertEquals(true,  eval("(let ((b (box (+ 1 2 3)))) (box-cas! b 6 9))", env))
        assertEquals(false, eval("(let ((b (box (+ 1 2 3)))) (box-cas! b (* 3 3) 12))", env))
    }

    @Test
    fun testEvalSwap() {
        assertEquals(7L,  eval("(let ((b (box (+ 1 2 3)))) (swap! b inc))", env))
        assertEquals(6L,  eval("(let ((b (box (+ 1 2 3)))) (swap! b identity))", env))
        assertEquals(7L,  eval("(let ((b (box (+ 1 2 3)))) (swap! b (lambda (n) (+ n 1)) ))", env))
        assertEquals(20L, eval("(let ((b (box (+ 1 2 3)))) (swap! b (lambda (n1 n2 n3) (+ n1 n2 n3)) 7 7))", env))
        assertEquals(20L, eval("(let ((b (box (+ 1 2 3)))) (swap! b (lambda (n . rest) (apply + n rest)) 7 7))", env))
    }

    @Test
    fun testEvalIsBox() {
        assertEquals(true,  eval("(box?  (box  nil))", env))
        assertEquals(true,  eval("(atom? (box  nil))", env))
        assertEquals(true,  eval("(box?  (atom nil))", env))
        assertEquals(true,  eval("(atom? (atom nil))", env))
        assertEquals(false, eval("(box?  1)", env))
        assertEquals(false, eval("(atom? 1)", env))
        assertEquals(false, eval("(box?  (delay 1))", env))
        assertEquals(false, eval("(atom? (promise))", env))
    }
}
