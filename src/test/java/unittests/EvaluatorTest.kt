package unittests

import core.Repl
import core.environment.DefaultEnvironment
import core.exceptions.IllegalSyntaxException
import core.procedures.io.Display
import core.scm.Cons.Companion.list
import core.scm.OutputPort
import core.scm.Symbol
import core.scm.Void
import org.junit.Assert.*
import org.junit.Test
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.lang.Boolean.FALSE
import java.lang.Boolean.TRUE

class EvaluatorTest : AbstractTest() {

    @Test
    fun testEvalEmptyList() {
        try {
            eval("(())", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("eval: bad syntax (illegal empty application) in form: ()", e.message)
        }
    }

    @Test
    fun testEvalLocalState() {
        val lenv = DefaultEnvironment()
        eval("(define (make-withdraw balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) \"Insufficient funds\")))", lenv)
        eval("(define W1 (make-withdraw 100))", lenv)
        eval("(define W2 (make-withdraw 100))", lenv)
        assertEquals(50L, eval("(W1 50)", lenv))
        assertEquals(30L, eval("(W2 70)", lenv))
        assertEquals("Insufficient funds", eval("(W2 40)", lenv))
        assertEquals(10L, eval("(W1 40)", lenv))

        eval("(define a 999)", lenv)
        assertEquals(5L, eval("(begin (define a 10) (set! a 5) a)", lenv))
        assertEquals(20L, eval("(let ((a 5)) (set! a 10) (let () (set! a 15) (let () (+ a 5))))", lenv))

        eval("(define x 2)", lenv)
        eval("(define y 10)", lenv)
        eval("(define multiply (lambda (x y) (* x y)))", lenv)
        assertEquals(12L, eval("(+ x y)", lenv))
        assertEquals(100L, eval("(multiply y 10)", lenv))

        eval("(define x 10)", lenv)
        eval("(set! x 20)", lenv)
        assertEquals(20L, eval("x", lenv))
        eval("(define add (lambda (x y) (set! x (+ x y)) x))", lenv)
        assertEquals(110L, eval("(add 10 100)", lenv))

        eval("(define x 4)", lenv)
        eval("(define y 5)", lenv)
        assertEquals(2L, eval("(let ((x 1) (y 2)) (* x y))", lenv))
        assertEquals(20L, eval("(* x y)", lenv))
    }

    @Test
    fun testEvalBooleanPredicates() {
        assertEquals(TRUE, eval("(boolean? #f)", env))
        assertEquals(TRUE, eval("(boolean? #t)", env))
        assertEquals(TRUE, eval("(boolean? (= 1 1))", env))
        assertEquals(FALSE, eval("(boolean? #\\f)", env))
        assertEquals(FALSE, eval("(boolean? 1)", env))
        assertEquals(TRUE, eval("(true? #t)", env))
        assertEquals(FALSE, eval("(true? #f)", env))
        assertEquals(FALSE, eval("(true? 1)", env))
        assertEquals(TRUE, eval("(true? (= 1 1))", env))
        assertEquals(FALSE, eval("(false? #t)", env))
        assertEquals(TRUE, eval("(false? #f)", env))
        assertEquals(FALSE, eval("(false? 1)", env))
        assertEquals(FALSE, eval("(false? (= 1 1))", env))
        assertEquals(TRUE, eval("(false? (= 1 2))", env))
    }

    @Test
    fun testEvalNegation() {
        assertEquals(FALSE, eval("(not #t)", env))
        assertEquals(TRUE, eval("(not #f)", env))
        assertEquals(TRUE, eval("(not (= 1 2 1))", env))
        assertEquals(FALSE, eval("(not (= 1 1 1))", env))
    }

    // Equivalence
    @Test
    fun testEvalEq() {
        assertEquals(TRUE, eval("(eq? '() '())", env))
        assertEquals(TRUE, eval("(eq? 1 1)", env))
        assertEquals(FALSE, eval("(eq? 1 2)", env))
        // interned immutable strings are the same objects
        assertEquals(TRUE, eval("(eq? \"1\" \"1\")", env))
        // mutable strings are not interned
        assertEquals(FALSE, eval("(eq? (string #\\a) (string #\\a))", env))
    }

    @Test
    fun testEvalEqv() {
        assertEquals(TRUE, eval("(eqv? '() '())", env))
        assertEquals(TRUE, eval("(eqv? 1 1)", env))
        assertEquals(FALSE, eval("(eqv? 1 2)", env))
        // interned immutable strings are the same objects
        assertEquals(TRUE, eval("(eqv? \"1\" \"1\")", env))
        assertEquals(TRUE, eval("(eqv? \"a\" \"a\")", env))
        // mutable strings are not interned
        assertEquals(FALSE, eval("(eqv? (string #\\a) (string #\\a))", env))
    }

    @Test
    fun testEvalEqual() {
        assertEquals(TRUE, eval("(equal? '() '())", env))
        assertEquals(TRUE, eval("(equal? '(1 2 3) '( 1 2 3))", env))
        assertEquals(FALSE, eval("(equal? '(1 2 3 5) '( 1 2 3))", env))
        assertEquals(TRUE, eval("(equal? 1 1)", env))
        assertEquals(FALSE, eval("(equal? 1 2)", env))
        assertEquals(TRUE, eval("(equal? \"1fe\" \"1fe\")", env))
    }

    @Test
    fun testEvalDisplay() {

        val baos = ByteArrayOutputStream()
        val old = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(PrintStream(baos))

        val tempEnv = DefaultEnvironment()
        /* Eval lib procedures */
        for (proc in tempEnv.libraryProcedures) {
            eval(proc, tempEnv)
        }
        tempEnv.put(Symbol.intern("display"), Display())

        eval("(display 123)", tempEnv)
        assertEquals("123", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display -123.25)", tempEnv)
        assertEquals("-123.25", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display \"test string\")", tempEnv)
        assertEquals("test string", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display '())", tempEnv)
        assertEquals("()", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display '(1 2 3 #\\A (1 . 2)))", tempEnv)
        assertEquals("(1 2 3 #\\A (1 . 2))", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display (list 1 2 3 #\\A (cons 1 2) (list 1 2 3)))", tempEnv)
        assertEquals("(1 2 3 #\\A (1 . 2) (1 2 3))", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display (string->list \"Hello\"))", tempEnv)
        assertEquals("(#\\H #\\e #\\l #\\l #\\o)", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display (cdr (cdr '(1 2 3 4 5 6))))", tempEnv)
        assertEquals("(3 4 5 6)", baos.toString().trim { it <= ' ' })
        baos.reset()

        Repl.currentOutputPort = old
    }

    @Test
    fun testEvalMap() {
        assertEquals(list(2L, 3L, 4L, 5L, 6L), eval("(map (lambda (n) (+ n 1)) '(1 2 3 4 5))", env))
        assertEquals(list(11L, 102L, 1003L, 10004L),
                eval("(map (lambda (number1 number2) (+ number1 number2)) '(1 2 3 4) '(10 100 1000 10000))", env))

        assertEquals(list(1L, 4L), eval("(map car '((1 2 3) (4 5 6)))", env))
        assertEquals(list(3, 2, 4), eval("(map length '( (1 4 0) (C G) (\"The\" \"Way\" \"Out\" \"Is\") ))", env))
        assertEquals(list(12L, 15L, 16L), eval("(map * '(2 3 4) '(6 5 4))", env))
        assertEquals(list(0, 1, 2), eval("(map length '(() (a) (a b)))", env))
    }

    @Test
    fun testEvalApply() {
        assertEquals(32L, eval("(apply + 1 -2 3 '(10 20))", env))
        assertEquals(list(list(Symbol.intern("a"), 1L), list(Symbol.intern("b"), 2L), list(
                Symbol.intern("c"), 3L)),
                eval("(apply map list '((a b c) (1 2 3)))", env))

        eval("(define (sqr x) (* x x))", env)
        assertEquals(385L, eval("(apply + (map sqr '(1 2 3 4 5 6 7 8 9 10)))", env))
    }

    @Test
    fun testForEach() {
        assertEquals(Void, eval("(for-each length '(() (a) (a b)))", env))
    }

    @Test
    fun testManyArgs() {
        assertEquals(524794L, eval("(apply + (map inc (range 3 1024)))", env))
    }

    @Test
    fun testEvalSet() {
        assertTrue(eval("#{}", env) is Set<*>)
        assertEquals(1, eval("(count #{(+ 1 2)})", env))
        assertEquals(3L, eval("(first #{(+ 1 2)})", env))
        assertEquals(15L, eval("(first #{(apply + [1 2 3 4 5])})", env))
    }
}
