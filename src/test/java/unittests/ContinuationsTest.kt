package unittests

import core.Repl
import core.exceptions.ReentrantContinuationException
import core.scm.Continuation
import core.scm.OutputPort
import core.scm.Symbol
import org.junit.Test

import java.io.ByteArrayOutputStream
import java.io.PrintStream

import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class ContinuationsTest : AbstractTest() {

    @Test
    fun testListAdd() {
        val listadd = "(define (lstadd1 lst)" +
                      "  (call/cc (lambda (exit)" +
                      "    (let loop ((lst lst))" +
                      "       (cond ((pair? lst) (cons (add1 (car lst)) (loop (cdr lst))))" +
                      "             ((empty? lst) '())" +
                      "             (else (exit #f)))))))"
        eval(listadd)
        assertEquals(listOf(2L, 3L, 4L), eval("(into '() (lstadd1 '(1 2 3)))"))
        assertEquals(false, eval("(lstadd1 '(1 2 . 3))"))
    }

    @Test
    fun testCC() {
        val cc = "(define (cc) (call-with-current-continuation (lambda (cc) (cc cc))))"
        eval(cc)
        assertEquals(Continuation::class.java, eval("(class-of (cc))"))
        try {
            eval("((cc) display)")
            fail()
        } catch (e: ReentrantContinuationException) {
            // success
        }
    }

    @Test
    fun testYingYang() {
        val baos = ByteArrayOutputStream()
        val old = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(PrintStream(baos))

        val yingyang = "(let* ((yin  ((lambda (cc) (display #\\@) cc) (call-with-current-continuation (lambda (c) c))))" +
                       "       (yang ((lambda (cc) (display #\\*) cc) (call-with-current-continuation (lambda (c) c)))))" +
                       "    (yin yang))"
        try {
            eval(yingyang)
            fail()
        } catch (ex: ReentrantContinuationException) {
            assertEquals("@*", baos.toString().trim { it <= ' ' })
        }
        Repl.currentOutputPort = old
    }

    @Test
    fun testWikiExample() {
        val baos = ByteArrayOutputStream()
        val old = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(PrintStream(baos))

        val example = "(define (f return)" +
                      "  (return 2)" +
                      "  3)"

        eval(example)
        eval("(display (f (lambda (x) x))) ; displays 3")
        eval("(display (call-with-current-continuation f)) ; displays 2")
        assertEquals("32", baos.toString().trim { it <= ' ' })

        Repl.currentOutputPort = old
    }

    @Test
    fun testContinuationExample() {
        val example = "(let ((cont #f))" +
                       "  (letrec ((x (call-with-current-continuation (lambda (c) (set! cont c) 0)))" +
                       "           (y (call-with-current-continuation (lambda (c) (set! cont c) 0))))" +
                       "    (if cont" +
                       "        (let ((c cont))" +
                       "          (set! cont #f)" +
                       "          (set! x 1)" +
                       "          (set! y 1)" +
                       "          (c 0))" +
                       "        (+ x y))))"
        try {
            eval(example)
            fail()
        } catch (ex: ReentrantContinuationException) {
            // success
        }
    }

    @Test
    fun testDynamicWind() {
        eval("(define x 'normal-binding)")
        val dw = "(define a-cont" +
                 "  (call-with-current-continuation" +
                 "   (lambda (escape)" +
                 "     (let ((old-x x))" +
                 "       (dynamic-wind" +
                 "           (lambda () (set! x 'special-binding))" +
                 "           (lambda () (display x) (newline)" +
                 "                      (call-with-current-continuation escape)" +
                 "                      (display x) (newline)" +
                 "                      x)" +
                 "           (lambda () (set! x old-x)))))))"

        val baos = ByteArrayOutputStream()
        val old = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(PrintStream(baos))

        eval(dw)
        assertEquals("special-binding", baos.toString().trim { it <= ' ' })
        assertEquals(Continuation::class.java, eval("(class-of a-cont)"))
        assertEquals(Symbol.intern("normal-binding"), eval("x"))
        try {
            eval("(a-cont #f)")
            fail()
        } catch (e: ReentrantContinuationException) {
            // success
        }
        Repl.currentOutputPort = old
    }
}
