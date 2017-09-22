package unittests

import core.Repl
import core.environment.DefaultEnvironment
import core.exceptions.IllegalSyntaxException
import core.procedures.io.Display
import core.scm.OutputPort
import core.scm.Symbol
import org.junit.Assert.*
import org.junit.Test
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.math.BigInteger

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
        assertEquals(true,  eval("(boolean? #f)", env))
        assertEquals(true,  eval("(boolean? #t)", env))
        assertEquals(true,  eval("(boolean? (= 1 1))", env))
        assertEquals(false, eval("(boolean? #\\f)", env))
        assertEquals(false, eval("(boolean? 1)", env))
        assertEquals(true,  eval("(true? #t)", env))
        assertEquals(false, eval("(true? #f)", env))
        assertEquals(false, eval("(true? 1)", env))
        assertEquals(true,  eval("(true? (= 1 1))", env))
        assertEquals(false, eval("(false? #t)", env))
        assertEquals(true,  eval("(false? #f)", env))
        assertEquals(false, eval("(false? 1)", env))
        assertEquals(false, eval("(false? (= 1 1))", env))
        assertEquals(true,  eval("(false? (= 1 2))", env))
    }

    @Test
    fun testEvalNegation() {
        assertEquals(false, eval("(not #t)", env))
        assertEquals(true,  eval("(not #f)", env))
        assertEquals(true,  eval("(not (= 1 2 1))", env))
        assertEquals(false, eval("(not (= 1 1 1))", env))
    }

    // Equivalence
    @Test
    fun testEvalEq() {
        assertEquals(true, eval("(eq? '() '())", env))
        assertEquals(true, eval("(eq? 1 1)", env))
        assertEquals(false, eval("(eq? 1 2)", env))
        // interned immutable strings are the same objects
        assertEquals(true, eval("(eq? \"1\" \"1\")", env))
        // mutable strings are not interned
        assertEquals(false, eval("(eq? (string #\\a) (string #\\a))", env))
    }

    @Test
    fun testEvalEqv() {
        assertEquals(true, eval("(eqv? '() '())", env))
        assertEquals(true, eval("(eqv? 1 1)", env))
        assertEquals(false, eval("(eqv? 1 2)", env))
        // interned immutable strings are the same objects
        assertEquals(true, eval("(eqv? \"1\" \"1\")", env))
        assertEquals(true, eval("(eqv? \"a\" \"a\")", env))
        // mutable strings are not interned
        assertEquals(false, eval("(eqv? (string #\\a) (string #\\a))", env))
    }

    @Test
    fun testEvalEqual() {
        assertEquals(true,  eval("(equal? '() '())", env))
        assertEquals(true,  eval("(equal? '(1 2 3) '( 1 2 3))", env))
        assertEquals(false, eval("(equal? '(1 2 3 5) '( 1 2 3))", env))
        assertEquals(true,  eval("(equal? 1 1)", env))
        assertEquals(false, eval("(equal? 1 2)", env))
        assertEquals(true,  eval("(equal? \"1fe\" \"1fe\")", env))
    }

    @Test
    fun testEvalDisplay() {

        val baos = ByteArrayOutputStream()
        val old = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(PrintStream(baos))

        val tempEnv = DefaultEnvironment()
        tempEnv.libraryProcedures.forEach { eval(it, tempEnv) }
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
        assertEquals(listOf(2L, 3L, 4L, 5L, 6L), eval("(into '() (map (lambda (n) (+ n 1)) '(1 2 3 4 5)))", env))
        assertEquals(listOf(11L, 102L, 1003L, 10004L),
                eval("(into '() (map (lambda (number1 number2) (+ number1 number2)) '(1 2 3 4) '(10 100 1000 10000)))", env))

        assertEquals(listOf(1L, 4L),  eval("(into '() (map car '((1 2 3) (4 5 6))))", env))
        assertEquals(listOf(3, 2, 4), eval("(into '() (map length '( (1 4 0) (C G) (\"The\" \"Way\" \"Out\" \"Is\") )))", env))
        assertEquals(listOf(12L, 15L, 16L), eval("(into '() (map * '(2 3 4) '(6 5 4)))", env))
        assertEquals(listOf(0, 1, 2), eval("(into '() (map length '(() (a) (a b))))", env))
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), eval("(into '() (take 5 (map inc (range))))", env))
        assertEquals(listOf(0L, 0L, 1L, 0L, 1L, 2L, 0L, 1L, 2L, 3L), eval("(flatten (map range (range 5)))", env))
    }

    @Test
    fun testEvalApply() {
        assertEquals(32L, eval("(apply + 1 -2 3 '(10 20))", env))
        assertEquals(listOf(listOf(Symbol.intern("a"), 1L), listOf(Symbol.intern("b"), 2L), listOf(
                Symbol.intern("c"), 3L)),
                eval("(into '() (apply map list '((a b c) (1 2 3))))", env))

        eval("(define (sqr x) (* x x))", env)
        assertEquals(385L, eval("(apply + (map sqr '(1 2 3 4 5 6 7 8 9 10)))", env))
    }

    @Test
    fun testForEach() {
        assertEquals(Unit, eval("(for-each length '(() (a) (a b)))", env))
        assertEquals(45L,  eval("(let ((a 0)) (for-each (lambda (n) (set! a (+ a n))) (range 10)) a)", env))
        assertEquals(10L,  eval("(begin (define a 0) (for-each (lambda (n) (set! a (inc a))) (range 10)) a)", env))
    }

    @Test
    fun testManyArgs() {
        assertEquals(523776L, eval("(+ ${(1..1023).joinToString(" ")})", env))
    }

    @Test
    fun testEvalSet() {
        assertEquals(true,  eval("#{}", env) is Set<*>)
        assertEquals(1,     eval("(count #{(+ 1 2)})", env))
        assertEquals(3L,    eval("(first #{(+ 1 2)})", env))
        assertEquals(15L,   eval("(first #{(apply + [1 2 3 4 5])})", env))
        assertEquals(null,  eval("(#{nil} nil)", env))
        assertEquals(null,  eval("(#{1 2 3} false)", env))
        assertEquals(false, eval("(#{false} false)", env))
        assertEquals(1L,    eval("(#{1 2 3} 1)", env))
        assertEquals(null,  eval("(#{1 2 3} 10)", env))
        assertEquals("str", eval("(#{\"a\" \"str\" 1} \"str\")", env))
    }

    @Test
    fun testMemoize() {
        val memoizedFibonacci = "(def m-fib (memoize (fn (n)" +
                                "             (cond" +
                                "              ((= n 0) 1)" +
                                "              ((= n 1) 1)" +
                                "              (else (+ (m-fib (dec n)) (m-fib (- n 2))))))))"
        eval(memoizedFibonacci, env)
        assertEquals(BigInteger("573147844013817084101"), eval("(m-fib 100)", env))

//        val fib500 = "225591516161936330872512695036072072046011324913758190588638866418474627738686883405015987052796968498626"
//        val fib1000 ="703303677114228158218352548771835497701812698363587327426049050871545371181969335797422494945" +
//                     "626117334877504492417659910881863632654502236471060120533741212738673391111981393731255987676" +
//                     "90091902245245323403501"
//        assertEquals(BigInteger(fib500), eval("(m-fib 500)", env))
//        assertEquals(BigInteger(fib1000), eval("(m-fib 1000)", env))
    }

    @Test
    fun testComplement() {
        val tempEnv = DefaultEnvironment().apply { libraryProcedures.forEach { eval(it, this) } }

        eval("(def c (negate identity))",  tempEnv)
        assertEquals(true,  eval("(c #f)", tempEnv))
        assertEquals(false, eval("(c #t)", tempEnv))
        assertEquals(false, eval("(c  1)", tempEnv))
        assertEquals(false, eval("(c 'a)", tempEnv))

        eval("(def c (complement zero?))",   tempEnv)
        assertEquals(false, eval("(c 0)",    tempEnv))
        assertEquals(true,  eval("(c 1)",    tempEnv))
        assertEquals(true,  eval("(c 1+2i)", tempEnv))

        eval("(def not-empty? (complement empty?))", tempEnv)
        assertEquals(false, eval("(not-empty? [])", tempEnv))
        assertEquals(true,  eval("(not-empty? [1 2 3])", tempEnv))
    }

    @Test
    fun testPartial() {
        eval("(def hundred-times (partial * 100))", env)
        assertEquals(100L, eval("(hundred-times)", env))
        assertEquals(500L, eval("(hundred-times 5)", env))
        assertEquals(600L, eval("(hundred-times 1 2 3)", env))

        eval("(def is-string (partial instance? String))", env)
        assertEquals(true,  eval("(is-string \"test\")", env))
        assertEquals(false, eval("(is-string 12345678)", env))
    }
}
