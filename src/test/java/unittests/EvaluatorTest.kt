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
            eval("(())")
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("eval: bad syntax (illegal empty application) in form: ()", e.message)
        }
    }

    @Test
    fun testEvalLocalState() {
        val lenv = DefaultEnvironment()
        eval("(define (make-withdraw balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) \"Insufficient funds\")))")
        eval("(define W1 (make-withdraw 100))")
        eval("(define W2 (make-withdraw 100))")
        assertEquals(50L, eval("(W1 50)"))
        assertEquals(30L, eval("(W2 70)"))
        assertEquals("Insufficient funds", eval("(W2 40)"))
        assertEquals(10L, eval("(W1 40)"))

        eval("(define a 999)")
        assertEquals(5L, eval("(begin (define a 10) (set! a 5) a)"))
        assertEquals(20L, eval("(let ((a 5)) (set! a 10) (let () (set! a 15) (let () (+ a 5))))"))

        eval("(define x 2)")
        eval("(define y 10)")
        eval("(define multiply (lambda (x y) (* x y)))")
        assertEquals(12L, eval("(+ x y)"))
        assertEquals(100L, eval("(multiply y 10)"))

        eval("(define x 10)")
        eval("(set! x 20)")
        assertEquals(20L, eval("x"))
        eval("(define add (lambda (x y) (set! x (+ x y)) x))")
        assertEquals(110L, eval("(add 10 100)"))

        eval("(define x 4)")
        eval("(define y 5)")
        assertEquals(2L, eval("(let ((x 1) (y 2)) (* x y))"))
        assertEquals(20L, eval("(* x y)"))
    }

    @Test
    fun testEvalBooleanPredicates() {
        assertEquals(true,  eval("(boolean? #f)"))
        assertEquals(true,  eval("(boolean? #t)"))
        assertEquals(true,  eval("(boolean? (= 1 1))"))
        assertEquals(false, eval("(boolean? #\\f)"))
        assertEquals(false, eval("(boolean? 1)"))
        assertEquals(true,  eval("(true? #t)"))
        assertEquals(false, eval("(true? #f)"))
        assertEquals(false, eval("(true? 1)"))
        assertEquals(true,  eval("(true? (= 1 1))"))
        assertEquals(false, eval("(false? #t)"))
        assertEquals(true,  eval("(false? #f)"))
        assertEquals(false, eval("(false? 1)"))
        assertEquals(false, eval("(false? (= 1 1))"))
        assertEquals(true,  eval("(false? (= 1 2))"))
    }

    @Test
    fun testEvalNegation() {
        assertEquals(false, eval("(not #t)"))
        assertEquals(true,  eval("(not #f)"))
        assertEquals(true,  eval("(not (= 1 2 1))"))
        assertEquals(false, eval("(not (= 1 1 1))"))
    }

    // Equivalence
    @Test
    fun testEvalEq() {
        assertEquals(true, eval("(eq? '() '())"))
        assertEquals(true, eval("(eq? 1 1)"))
        assertEquals(false, eval("(eq? 1 2)"))
        // interned immutable strings are the same objects
        assertEquals(true, eval("(eq? \"1\" \"1\")"))
        // mutable strings are not interned
        assertEquals(false, eval("(eq? (string #\\a) (string #\\a))"))
    }

    @Test
    fun testEvalEqv() {
        assertEquals(true, eval("(eqv? '() '())"))
        assertEquals(true, eval("(eqv? 1 1)"))
        assertEquals(false, eval("(eqv? 1 2)"))
        // interned immutable strings are the same objects
        assertEquals(true, eval("(eqv? \"1\" \"1\")"))
        assertEquals(true, eval("(eqv? \"a\" \"a\")"))
        // mutable strings are not interned
        assertEquals(false, eval("(eqv? (string #\\a) (string #\\a))"))
    }

    @Test
    fun testEvalEqual() {
        assertEquals(true,  eval("(equal? '() '())"))
        assertEquals(true,  eval("(equal? '(1 2 3) '( 1 2 3))"))
        assertEquals(false, eval("(equal? '(1 2 3 5) '( 1 2 3))"))
        assertEquals(true,  eval("(equal? 1 1)"))
        assertEquals(false, eval("(equal? 1 2)"))
        assertEquals(true,  eval("(equal? \"1fe\" \"1fe\")"))
    }

    @Test
    fun testEvalDisplay() {

        val baos = ByteArrayOutputStream()
        val old = Repl.currentOutputPort
        Repl.currentOutputPort = OutputPort(PrintStream(baos))

        eval("(display 123)")
        assertEquals("123", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display -123.25)")
        assertEquals("-123.25", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display \"test string\")")
        assertEquals("test string", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display '())")
        assertEquals("()", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display '(1 2 3 #\\A (1 . 2)))")
        assertEquals("(1 2 3 #\\A (1 . 2))", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display (list 1 2 3 #\\A (cons 1 2) (list 1 2 3)))")
        assertEquals("(1 2 3 #\\A (1 . 2) (1 2 3))", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display (string->list \"Hello\"))")
        assertEquals("(#\\H #\\e #\\l #\\l #\\o)", baos.toString().trim { it <= ' ' })
        baos.reset()

        eval("(display (cdr (cdr '(1 2 3 4 5 6))))")
        assertEquals("(3 4 5 6)", baos.toString().trim { it <= ' ' })
        baos.reset()

        Repl.currentOutputPort = old
    }

    @Test
    fun testEvalMap() {
        assertEquals(listOf(2L, 3L, 4L, 5L, 6L), eval("(into '() (map (lambda (n) (+ n 1)) '(1 2 3 4 5)))"))
        assertEquals(listOf(11L, 102L, 1003L, 10004L),
                eval("(into '() (map (lambda (number1 number2) (+ number1 number2)) '(1 2 3 4) '(10 100 1000 10000)))"))

        assertEquals(listOf(1L, 4L),  eval("(into '() (map car '((1 2 3) (4 5 6))))"))
        assertEquals(listOf(3, 2, 4), eval("(into '() (map length '( (1 4 0) (C G) (\"The\" \"Way\" \"Out\" \"Is\") )))"))
        assertEquals(listOf(12L, 15L, 16L), eval("(into '() (map * '(2 3 4) '(6 5 4)))"))
        assertEquals(listOf(0, 1, 2), eval("(into '() (map length '(() (a) (a b))))"))
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), eval("(into '() (take 5 (map inc (range))))"))
        assertEquals(listOf(0L, 0L, 1L, 0L, 1L, 2L, 0L, 1L, 2L, 3L), eval("(into '() (flatten (map range (range 5))))"))
    }

    @Test
    fun testEvalApply() {
        assertEquals(32L, eval("(apply + 1 -2 3 '(10 20))"))
        assertEquals(listOf(listOf(Symbol.intern("a"), 1L), listOf(Symbol.intern("b"), 2L), listOf(
                Symbol.intern("c"), 3L)),
                eval("(into '() (apply map list '((a b c) (1 2 3))))"))

        eval("(define (sqr x) (* x x))")
        assertEquals(385L, eval("(apply + (map sqr '(1 2 3 4 5 6 7 8 9 10)))"))
    }

    @Test
    fun testForEach() {
        assertEquals(Unit, eval("(for-each length '(() (a) (a b)))"))
        assertEquals(45L,  eval("(let ((a 0)) (for-each (lambda (n) (set! a (+ a n))) (range 10)) a)"))
        assertEquals(10L,  eval("(begin (define a 0) (for-each (lambda (n) (set! a (inc a))) (range 10)) a)"))
    }

    @Test
    fun testManyArgs() {
        assertEquals(523776L, eval("(+ ${(1..1023).joinToString(" ")})"))
    }

    @Test
    fun testEvalSet() {
        assertEquals(true,  eval("#{}") is Set<*>)
        assertEquals(1,     eval("(count #{(+ 1 2)})"))
        assertEquals(3L,    eval("(first #{(+ 1 2)})"))
        assertEquals(15L,   eval("(first #{(apply + [1 2 3 4 5])})"))
        assertEquals(null,  eval("(#{nil} nil)"))
        assertEquals(null,  eval("(#{1 2 3} false)"))
        assertEquals(false, eval("(#{false} false)"))
        assertEquals(1L,    eval("(#{1 2 3} 1)"))
        assertEquals(null,  eval("(#{1 2 3} 10)"))
        assertEquals("str", eval("(#{\"a\" \"str\" 1} \"str\")"))
    }

    @Test
    fun testMemoize() {
        val memoizedFibonacci = "(def m-fib (memoize (fn (n)" +
                                "             (cond" +
                                "              ((= n 0) 1)" +
                                "              ((= n 1) 1)" +
                                "              (else (+ (m-fib (dec n)) (m-fib (- n 2))))))))"
        eval(memoizedFibonacci)
        assertEquals(BigInteger("573147844013817084101"), eval("(m-fib 100)"))

//        val fib500 = "225591516161936330872512695036072072046011324913758190588638866418474627738686883405015987052796968498626"
//        val fib1000 ="703303677114228158218352548771835497701812698363587327426049050871545371181969335797422494945" +
//                     "626117334877504492417659910881863632654502236471060120533741212738673391111981393731255987676" +
//                     "90091902245245323403501"
//        assertEquals(BigInteger(fib500), eval("(m-fib 500)"))
//        assertEquals(BigInteger(fib1000), eval("(m-fib 1000)"))
    }

    @Test
    fun testComplement() {
        eval("(def c (negate identity))")
        assertEquals(true,  eval("(c #f)"))
        assertEquals(false, eval("(c #t)"))
        assertEquals(false, eval("(c  1)"))
        assertEquals(false, eval("(c 'a)"))

        eval("(def c (complement zero?))")
        assertEquals(false, eval("(c 0)"))
        assertEquals(true,  eval("(c 1)"))
        assertEquals(true,  eval("(c 1+2i)"))

        eval("(def not-empty? (complement empty?))")
        assertEquals(false, eval("(not-empty? [])"))
        assertEquals(true,  eval("(not-empty? [1 2 3])"))
    }

    @Test
    fun testPartial() {
        eval("(def hundred-times (partial * 100))")
        assertEquals(100L, eval("(hundred-times)"))
        assertEquals(500L, eval("(hundred-times 5)"))
        assertEquals(600L, eval("(hundred-times 1 2 3)"))

        eval("(def is-string (partial instance? String))")
        assertEquals(true,  eval("(is-string \"test\")"))
        assertEquals(false, eval("(is-string 12345678)"))
    }
}
