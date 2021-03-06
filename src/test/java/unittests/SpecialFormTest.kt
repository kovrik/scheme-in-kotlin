package unittests

import core.Repl
import core.environment.DefaultEnvironment
import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException
import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.io.Display
import core.procedures.math.Addition
import core.scm.*
import core.scm.specialforms.Quasiquote
import core.scm.specialforms.Quote
import core.scm.specialforms.Unquote
import core.scm.specialforms.UnquoteSplicing
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.test.assertTrue

class SpecialFormTest : AbstractTest() {

    @Test
    fun testEvalImplicitBegin() {
        assertEquals(3L, eval("((lambda () 1 2 (+ 1 2)))", env))
        assertEquals(3L, eval("(let    () 1 2 (+ 1 2))", env))
        assertEquals(3L, eval("(let*   () 1 2 (+ 1 2))", env))
        assertEquals(3L, eval("(letrec () 1 2 (+ 1 2))", env))
        eval("(define (a) 1 2 (+ 1 2))", env)
        assertEquals(3L, eval("(a)", env))
    }

    @Test
    fun testEvalMutualRecursion() {
        val f = "(define (F n) (if (= n 0) 1 (- n (M (F (- n 1))))))"
        val m = "(define (M n) (if (= n 0) 0 (- n (F (M (- n 1))))))"
        eval(f, env)
        eval(m, env)

        val fs = longArrayOf(1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13)
        fs.indices.forEach { i -> assertEquals(fs[i], eval("(F $i)", env)) }

        val ms = longArrayOf(0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12)
        ms.indices.forEach { i -> assertEquals(ms[i], eval("(M $i)", env)) }

        val letrec = "(letrec ((F (lambda (n) (if (= n 0) 1 (- n (M (F (- n 1)))))))" +
                     "(M (lambda (n) (if (= n 0) 0 (- n (F (M (- n 1))))))))" +
                     "(F 19))"
        assertEquals(12L, eval(letrec, env))
    }

    @Test
    fun testEvalDelayed() {
        assertEquals(1.0, eval("(force (delay 1.0))", env))
        assertEquals("test", eval("(force (delay \"test\"))", env))
        assertEquals(10L, eval("(force (delay (+ 5 2 (* 1 3))))", env))
        assertEquals(Delay::class.java, eval("(delay 1.0)", env)!!.javaClass)
        assertEquals(true, eval("(promise? (delay 1.0))", env))
        assertEquals(false, eval("(promise? (future 1.0))", env))
        assertEquals(false, eval("(future?  (delay 1.0))", env))
        assertEquals(true, eval("(future?  (future 1.0))", env))
        assertEquals(3L, eval("(force (delay (+ 1 2)))", env))
        assertEquals(listOf(3L, 3L), eval("(let ((p (delay (+ 1 2))))(list (force p) (force p)))", env))

        eval("(define perr (delay (error \"BOOM\")))", env)
        try {
            eval("(force perr)", env)
            fail()
        } catch (e: Error) {
            assertEquals("BOOM", e.message)
        }
        try {
            eval("(force perr)", env)
            fail()
        } catch (e: Error) {
            assertEquals("BOOM", e.message)
        }
        try {
            eval("(delay)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("delay: bad syntax in form: (delay)", e.message)
        }
    }

    @Test
    fun testEvalProcedure() {
        assertEquals(Closure::class.java, eval("(lambda () #t)", env)!!.javaClass)
        assertEquals(true, eval("((lambda () #t))", env))
        assertEquals(6L, eval("((lambda (n) (+ n 1)) 5)", env))

        eval("(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))", env)
        assertEquals(8L, eval("(fib 5)", env))

        assertEquals(6L, eval("((lambda (n) (+ n 1)) 5)", env))

        // rest arguments
        assertEquals(listOf(1L, 2L, 3L), eval("((lambda x x) 1 2 3)", env))
        assertEquals(emptyList<Nothing>(), eval("((lambda x x))", env))
        assertEquals(1L, eval("((lambda x (car x)) 1 2 3)", env))
        assertEquals(1L, eval("((lambda (f s . rs) f) 1 2 3 4)", env))
        assertEquals(2L, eval("((lambda (f s . rs) s) 1 2 3 4)", env))
        assertEquals(listOf(3L, 4L), eval("((lambda (f s . rs) rs) 1 2 3 4)", env))
    }

    @Test
    fun testEvalDefine() {
        eval("(define a 5)", env)
        assertEquals(5L, eval("a", env))
        assertEquals(Symbol.intern("b"), eval("(define b 7)", env))
        assertEquals(Symbol.intern("c"), eval("(define (c n) (+ n 7))", env))

        eval("(define edl (lambda (n) (+ n 1)))", env)
        assertEquals(2L, eval("(edl 1)", env))

        // variadic
        eval("(define edlv (lambda args args))", env)
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L), eval("(edlv 1 2 3 4 5)", env))

        // variadic define
        eval("(define (edv1 first second . rest) rest)", env)
        assertEquals(listOf(2L, 3L, 4L, 5L), eval("(edv1 0 1 2 3 4 5)", env))

        eval("(define (edv2 first second . rest) second)", env)
        assertEquals(1L, eval("(edv2 0 1 2 3 4 5)", env))

        try {
            eval("(define)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("define: bad syntax in form: (define)", e.message)
        }
        try {
            eval("(define 1)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("define: bad syntax in form: (define 1)", e.message)
        }
        try {
            eval("(define a)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("define: bad syntax in form: (define a)", e.message)
        }
        try {
            eval("(define a b c)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("define: bad syntax (multiple expressions after identifier) in form: (define a b c)", e.message)
        }
        // internal define
        assertEquals(45L, eval("(let ((x 5))(define foo (lambda (y) (bar x y)))(define bar (lambda (a b) (+ (* a b) a)))(foo (+ x 3)))", env))
        try {
            eval("(foo 5)", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }

        val d1 = "(define (test-internal-define)" +
                "  (let ((a 5) (b 7))" +
                "  (define (get-a) a)" +
                "  (define (get-b) b)" +
                "  (define (get-c) (+ (get-a) b))" +
                "  (+ (get-b) (get-c))))"
        eval(d1, env)
        assertEquals(19L, eval("(test-internal-define)", env))

        val d2 = "(define (test-internal-define2)" +
                "  (define (test2)" +
                "    (define (test4) 7)" +
                "    (define (test3) (test4))" +
                "    (+ 1 (test3)))" +
                "  (+ 1 (test2)))"
        eval(d2, env)
        assertEquals(9L, eval("(test-internal-define2)", env))
        assertEquals(listOf(3L, 4L, 5L), eval("((lambda (a b c . d) d) 0 1 2 3 4 5)", env))
        // TODO Check Definition context
    }

    @Test
    fun testEvalLambda() {
        try {
            eval("(lambda ())", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax in form: (lambda ())", e.message)
        }
        try {
            eval("(lambda 1 2 3 4)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax (bad argument sequence: (1)) in form: (lambda 1 2 3 4)", e.message)
        }
        try {
            eval("(lambda (1 2) 1)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax (not an identifier: 1) in form: (lambda (1 2) 1)", e.message)
        }
        try {
            eval("(lambda (a a) 1)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax (duplicate argument name: a) in form: (lambda (a a) 1)", e.message)
        }
    }

    @Test
    fun testEvalIf() {
        assertEquals(5L, eval("(if #t 5 0)", env))
        assertEquals(5L, eval("(if #f 0 5)", env))
        assertEquals(0L, eval("(if '() 0 5)", env))
        assertEquals(0L, eval("(if (not #f) 0 5)", env))
        assertEquals(5L, eval("(if (not (not (or #f #f))) 0 (+ 3 2))", env))
        assertEquals(Symbol.intern("yes"), eval("(if (> 3 2) 'yes 'no)", env))
        assertEquals(Symbol.intern("no"), eval("(if (> 2 3) 'yes 'no)", env))
        assertEquals(1L, eval("(if (> 3 2)(- 3 2)(+ 3 2))", env))
        assertEquals(10L, eval("(when #t 10)", env))
        assertEquals(Unit, eval("(when #f 5)", env))
        assertEquals(null, eval("(when #f)", env))
        assertEquals(null, eval("(when 5)", env))
        try {
            eval("(if)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("if: bad syntax (has 0 parts after keyword) in form: (if)", e.message)
        }
        try {
            eval("(if 1)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("if: bad syntax (has 1 parts after keyword) in form: (if 1)", e.message)
        }
        try {
            eval("(if 1 2 3 4 5)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("if: bad syntax (has 5 parts after keyword) in form: (if 1 2 3 4 5)", e.message)
        }
    }

    @Test
    fun testEvalQuote() {
        assertEquals(0L, eval("'0", env))
        assertEquals("test", eval("'\"test\"", env))
        assertEquals(listOf(Quote.symbol, "test"), eval("''\"test\"", env))
        assertEquals(listOf(Symbol.intern("+"), 1L, 2L), eval("'(+ 1 2)", env))
        assertEquals(Symbol.intern("0eab"), eval("'0eab", env))
        assertEquals(Symbol.intern("000eab"), eval("'000eab", env))
    }

    @Test
    fun testEvalDottedPair() {
        assertEquals(2L, eval("(car (cdr '(1 2 3 . (2 3 4))))", env))
        assertEquals(Pair(1L, 2L), eval("'(1 . 2)", env))
        assertEquals(Pair(1L, Pair(2L, Pair(3L, 4L))), eval("'(1 2 3 . 4)", env))
        assertEquals(6L, eval("(+ . (1 2 3))", env))
        assertEquals(8L, eval("(+ . (2 (+ . (1 2 3))))", env))
        assertEquals(6L, eval("(+ . (1 . (2 3)))", env))
        assertEquals(6L, eval("(+ . (1 . (2 . (3))))", env))
        assertEquals(6L, eval("(+ . (1 . (2 . (3 . ()))))", env))
        val illegals = arrayOf("'(1 2 3 . 4 5)", "(+ . 1)", "(+ . (2 . (3 . 4)))")
        illegals.forEach {
            try {
                eval(it, env)
                fail()
            } catch (e: IllegalSyntaxException) {
                // expected
            }
        }
        try {
            eval("( . 1 2 3 4 5)", env)
            fail()
        } catch (e: NoSuchMethodException) {
            assertEquals("reflector: unable to find matching method 2 in class java.lang.Long", e.message)
        }
    }

    @Test
    fun testEvalSet() {
        assertEquals(9L, eval("(let ((a 0)) (set! a 9) a)", env))
        assertEquals(19L, eval("(begin (define a 0) (set! a 9) (+ a 10))", env))
        try {
            eval("(begin (set! b 99) b)", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
    }

    @Test
    fun testEvalDo() {
        val doTest1 = "(do ((vec (make-vector 5))" +
                      "     (i 0 (+ i 1)))" +
                      "    ((= i 5) vec)" +
                      "  (vector-set! vec i i))"
        assertEquals(MutableVector(arrayOf(0L, 1L, 2L, 3L, 4L)), eval(doTest1, env))

        val doTest2 = "(let ((x '(1 3 5 7 9)))" +
                      "  (do ((x x (cdr x))" +
                      "       (sum 0 (+ sum (car x))))" +
                      "      ((empty? x) sum)))"
        assertEquals(25L, eval(doTest2, env))

        val doTest3 = "(do ((a 5)) ((= a 0) \"DONE\") (set! a (- a 1)))"
        assertEquals("DONE", eval(doTest3, env))

        assertEquals(Unit, eval("(do ((i 1 (add1 i))) ((> i 4)) (void i))", env))
        assertEquals("DONE", eval("(do ((i 1 (add1 i))) ((> i 4) \"DONE\") (void i))", env))

        try {
            eval("(do ((a 1) (b 2) (a 3)) (= 1 1) 5)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("let: bad syntax (duplicate identifier: a) in form: (do ((a 1) (b 2) (a 3)) (= 1 1) 5)", e.message)
        }

        /* Check that each iteration establishes bindings to fresh locations
         * See https://www.gnu.org/software/guile/manual/html_node/while-do.html */
        eval("(define lst '())", env)
        eval("(do ((i 1 (+ i 1)))" +
             "    ((> i 4))" +
             "  (set! lst (cons (lambda () i) lst)))", env)
        assertEquals(listOf(4L, 3L, 2L, 1L), eval("(into '() (map (lambda (proc) (proc)) lst))", env))
    }

    @Test
    fun testEvalLet() {
        assertEquals(124L, eval("(let ((c 123)) (+ c 1))", env))
        assertEquals(555L, eval("(let ((c 123) (b 432)) (+ c b))", env))
        try {
            eval("(let ((a 1) (b a) (c b)) c)", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
        try {
            eval("(let ((c 123) (c (+ 400 30 2))) (+ c b))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("let: bad syntax (duplicate identifier: c) in form: (let ((c 123) (c (+ 400 30 2))) (+ c b))",
                    e.message)
        }

        try {
            eval("(let ((c 123))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("let: bad syntax in form: (let ((c 123)))", e.message)
        }
        try {
            eval("(let ((z 1) (b (+ z 1))) b)", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
        try {
            eval("(let ((a a)) a)", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
    }

    @Test
    fun testEvalNamedLet() {
        assertEquals(120L, eval("(let fact ((n 5) (acc 1)) (if (= n 0) acc (fact (- n 1) (* acc n))))", env))
        assertEquals(12L, eval("(let t ((x 5) (y 7)) (+ x y))", env))
        try {
            eval("(let fact ((n 5) (n 1)) (if (= n 0) acc (fact (- n 1) (* n n))))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("let: bad syntax (duplicate identifier: n) in form: (let fact ((n 5) (n 1)) (if (= n 0) acc (fact (- n 1) (* n n))))", e.message)
        }

        eval("(define (duplicate pos lst)" +
             "  (let dup ((i 0)" +
             "            (lst lst))" +
             "   (cond" +
             "    ((= i pos) (cons (car lst) lst))" +
             "    (else (cons (car lst) (dup (+ i 1) (cdr lst)))))))", env)
        assertEquals(listOf("apple", "cheese burger!", "cheese burger!", "banana"),
                     eval("""(into '() (duplicate 1 (list "apple" "cheese burger!" "banana")))""", env))
    }

    @Test
    fun testEvalLetSeq() {
        assertEquals(2L, eval("(let* ((z 1) (b (+ z 1))) b)", env))
        assertEquals(1L, eval("(let* ((a 1) (b a) (c b)) c)", env))
        try {
            eval("(let* ((c 123)))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("let*: bad syntax in form: (let* ((c 123)))", e.message)
        }
        try {
            eval("(let* ((a a)) a)", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
    }

    @Test
    fun testEvalLetRec() {
        val letrec1 = "(letrec ((is-even? (lambda (n) (or (= n 0) (is-odd? (- n 1))))) " +
                "         (is-odd?  (lambda (n) (and (not (= n 0)) (is-even? (- n 1))))))" +
                "  (is-odd? 11))"
        assertEquals(true, eval(letrec1, env))
        assertEquals(1L, eval("(letrec ((a 1) (b a) (c b)) c)", env))
        try {
            eval("(letrec ((a a)) a)", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
        try {
            eval("(letrec ((a a)) (set! a 1) a)", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
        try {
            eval("(eq? (letrec ((a a)) a) (if #f 0) (letrec ((a a)) a))", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
    }

    @Test
    fun testEvalCond() {
        assertEquals(Unit, eval("(cond)", env))
        // "Invalid clause in subform "
        try {
            eval("(cond 1)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("cond: bad syntax (invalid clause in subform) in form: (cond 1)", e.message)
        }

        // "cond: else must be the last clause in subform"
        try {
            eval("(cond (else 1) (#t 5))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("cond: bad syntax (else must be the last clause in subform) in form: (cond (else 1) (#t 5))",
                    e.message)
        }

        assertEquals(1L, eval("(cond (#f 5) ((not #t) 7) (else 1))", env))
        assertEquals(7L, eval("(cond (#f 5) ((not #f) 7) (else 1))", env))

        assertEquals(Symbol.intern("greater"), eval("(cond ((> 3 2) 'greater)((< 3 2) 'less))", env))
        assertEquals(Symbol.intern("equal"), eval("(cond ((> 3 3) 'greater)((< 3 3) 'less)(else 'equal))", env))
    }

    @Test
    fun testEvalCase() {
        try {
            eval("(case)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("case: bad syntax (source expression failed to match any pattern) in form: (case)", e.message)
        }
        try {
            eval("(case 1 1)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("case: bad syntax (invalid clause in subform) in form: (case 1 1)", e.message)
        }
        try {
            eval("(case (* 2 3) (else 'prime) ((1 4 6 8 9) 'composite))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("case: bad syntax (else must be the last clause in subform) in form: (case (* 2 3) (else (quote prime)) ((1 4 6 8 9) (quote composite)))", e.message)
        }

        var caseform = "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"
        assertEquals(Symbol.intern("composite"), eval(caseform, env))

        caseform = "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 8 9) 'composite))"
        assertEquals(Unit, eval(caseform, env))

        caseform = "(case (* 2 3) ((2 3 5 7) 'prime) (else 'composite))"
        assertEquals(Symbol.intern("composite"), eval(caseform, env))
    }

    @Test
    fun testEvalAnd() {
        assertEquals(true,  eval("(and)", env))
        assertEquals(1L,    eval("(and 1)", env))
        assertEquals(true,  eval("(and (= 2 2) (> 2 1))", env))
        assertEquals(false, eval("(and (= 2 2) (< 2 1))", env))
        assertEquals(listOf<Any>(Symbol.intern("f"), Symbol.intern("g")), eval("(and 1 2 'c '(f g)) ", env))
    }

    @Test
    fun testEvalNand() {
        assertEquals(false, eval("(nand)", env))
        assertEquals(true,  eval("(nand #f #f)", env))
        assertEquals(true,  eval("(nand #f #t)", env))
        assertEquals(true,  eval("(nand #t #f)", env))
        assertEquals(false, eval("(nand #t #t)", env))
        assertEquals(true,  eval("(nand #f (error \"BOOM\"))", env))
    }

    @Test
    fun testEvalOr() {
        assertEquals(false, eval("(or)", env))
        assertEquals(true,  eval("(or (= 2 2) (> 2 1)) ", env))
        assertEquals(true,  eval("(or (= 2 2) (< 2 1))", env))
        assertEquals(false, eval("(or #f #f #f)", env))
        assertEquals(listOf<Any>(Symbol.intern("f"), Symbol.intern("g")), eval("(or '(f g) 1 2)", env))
    }

    @Test
    fun testEvalXor() {
        assertEquals(11L,   eval("(xor 11 #f)", env))
        assertEquals(22L,   eval("(xor #f 22)", env))
        assertEquals(false, eval("(xor 11 22)", env))
        assertEquals(false, eval("(xor #f #f)", env))
    }

    @Test
    fun testEvalNor() {
        assertEquals(true,  eval("(nor)", env))
        assertEquals(false, eval("(nor 1)", env))
        assertEquals(true,  eval("(nor #f #f)", env))
        assertEquals(false, eval("(nor #f 1)", env))
        assertEquals(false, eval("(nor 1 #f)", env))
        assertEquals(false, eval("(nor #t #T)", env))
        assertEquals(false, eval("(nor #t (error \"BOOM\"))", env))
    }

    @Test
    fun testEvalBegin() {
        assertEquals(Unit, eval("(begin)", env))
        assertEquals(Unit, eval("(begin (begin))", env))
        assertEquals(1L,   eval("(begin 1)", env))
        assertEquals(3L,   eval("(begin 1 2 3)", env))
        try {
            eval("(begin (set! x 5) (+ x 1))", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
        val old = Repl.currentOutputPort
        val baos = ByteArrayOutputStream()
        Repl.currentOutputPort = OutputPort(PrintStream(baos))
        val tempEnv = DefaultEnvironment()
        /* Eval lib procedures */
        tempEnv.libraryProcedures.forEach { eval(it, tempEnv) }
        tempEnv.put(Symbol.intern("display"), Display())
        assertEquals(Unit, eval("(begin (display \"4 plus 1 equals \")(display (+ 4 1)))", tempEnv))
        Repl.currentOutputPort = old
    }

    @Test
    fun testEvalClassOf() {
        // class-of
        assertEquals(Long::class.javaObjectType, eval("(class-of 1)", env))
        assertEquals(Long::class.javaObjectType, eval("(class-of -2341)", env))
        assertEquals(BigInteger::class.javaObjectType, eval("(class-of 9999999999999999999999999999999999)", env))
        assertEquals(Double::class.javaObjectType, eval("(class-of -1.0)", env))
        assertEquals(Double::class.javaObjectType, eval("(class-of -1.5)", env))
        assertEquals(BigDecimal::class.javaObjectType, eval("(class-of 9999999999999999999999999999999999.000)", env))
        assertEquals(BigDecimal::class.javaObjectType, eval("(class-of 9999999999999999999999999999999999.430)", env))
        assertEquals(Long::class.javaObjectType, eval("(class-of 1/1)", env))
        assertEquals(Ratio::class.javaObjectType, eval("(class-of -2341/345)", env))
        assertEquals(String::class.javaObjectType, eval("(class-of \"test\")", env))
        assertEquals(MutableString::class.javaObjectType, eval("(class-of (string #\\a))", env))
        assertEquals(Char::class.javaObjectType, eval("(class-of #\\A)", env))
        assertEquals(Symbol::class.javaObjectType, eval("(class-of 'test)", env))
        assertEquals(Class::class.javaObjectType, eval("(class-of (class-of 'test))", env))
        assertEquals(Vector::class.javaObjectType, eval("(class-of #(1 2 3))", env))
        assertEquals(Boolean::class.javaObjectType, eval("(class-of #t)", env))
        assertEquals(Boolean::class.javaObjectType, eval("(class-of (= 1 2))", env))
        assertEquals(Addition::class.javaObjectType, eval("(class-of +)", env))
        assertEquals(Closure::class.javaObjectType, eval("(class-of (lambda (n) n))", env))
        assertEquals(Delay::class.javaObjectType, eval("(class-of (delay (+ 1 2)))", env))
        assertEquals(null, eval("(class-of nil)", env))
        assertEquals(null, eval("(class-of (first '(nil)))", env))
    }

    @Test
    fun testEvalError() {
        // error
        try {
            eval("(error \"boom\")", env)!!.javaClass
            fail()
        } catch (e: Error) {
            assertEquals("boom", e.message)
        }
    }

    @Test
    fun testRedefineSpecialForms() {
        val tempEnv = DefaultEnvironment()
        eval("(define (and . args) #f)", tempEnv)
        eval("(define begin 5)", tempEnv)
        eval("(define if 4)", tempEnv)
        eval("(define quote 3)", tempEnv)
        eval("(define let 2)", tempEnv)
        eval("(define lambda 1)", tempEnv)
        assertEquals(15L, eval("(+ begin if quote let lambda)", tempEnv))
        assertEquals(3L, eval("(and 1 2 3)", env))
        assertEquals(false, eval("(and 1 2 3 4)", tempEnv))
    }

    @Test
    fun testQuasiquote() {
        assertEquals(1L, eval("(quasiquote 1)", env))
        assertEquals(1L, eval("`1", env))
        assertEquals(15.5, eval("(quasiquote 15.5)", env))
        assertEquals(15.5, eval("`15.5", env))
        assertEquals("test", eval("(quasiquote \"test\")", env))
        assertEquals("test", eval("`\"test\"", env))
        assertEquals(Quote.symbol, eval("(quasiquote quote)", env))
        assertEquals(listOf(Symbol.intern("+"), 1L, 2L), eval("`(+ 1 2)", env))
        assertEquals(3L, eval("`,(+ 1 2)", env))
        assertEquals(13L, eval("`,(+ 1 (* 3 4))", env))
        assertEquals(13L, eval("(quasiquote ,(+ 1 (* 3 4)))", env))
        assertEquals(13L, eval("(quasiquote (unquote (+ 1 (* 3 4))))", env))
        assertEquals(listOf(1L, 3L, 4L), eval("`(1 ,(+ 1 2) 4)", env))
        assertEquals(listOf(1L, listOf(Quasiquote.symbol, listOf(Unquote.symbol, listOf(
                Symbol.intern("+"), 1L, 5L))), 4L),
                eval("`(1 `,(+ 1 ,(+ 2 3)) 4)", env))

        assertEquals(listOf(1L, listOf(Quasiquote.symbol, listOf(Unquote.symbol, listOf(
                Symbol.intern("+"), 1L, MutableVector(arrayOf(
                Symbol.intern("+"), 2L, 3L))))), 4L),
                eval("`(1 `,(+ 1 ,'[+ 2 3]) 4)", env))

        assertEquals(listOf(Symbol.intern("list"), 3L, 4L), eval("`(list ,(+ 1 2) 4)", env))
        assertEquals(listOf(Symbol.intern("list"), Symbol.intern("a"), listOf(Quote.symbol, Symbol.intern("a"))),
                eval("(let ((name 'a)) `(list ,name ',name))", env))

        assertEquals(listOf(Symbol.intern("a"), 3L, 4L, 5L, 6L, Symbol.intern("b")),
                eval("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", env))

        assertEquals(Pair(listOf(Symbol.intern("foo"), 7L), Symbol.intern("cons")), eval("`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))", env))
        assertEquals(5L, eval("`,(+ 2 3)", env))

        assertEquals(listOf(1L, 2L, 3L), eval("`(1 ,@(list 2 3))", env))
        assertEquals(listOf(1L, 2L, 7L), eval("`(1 2 ,`,(+ 3 4))", env))
        assertEquals(1L, eval("`,`,`,`,`,1", env))
        assertEquals(1L, eval("`,`,`,`,`,`1", env))
        assertEquals(3L, eval("`,`,`,`,`,(+ 1 2)", env))
        assertEquals(listOf(Symbol.intern("+"), 1L, 2L), eval("`,`,`,`,`,`(+ 1 2)", env))

        assertEquals(MutableVector(arrayOf(1L, 5L)), eval("`[1 ,(+ 2 3)]", env))
        assertEquals(MutableVector(arrayOf(1L, listOf(Quasiquote.symbol, listOf(Unquote.symbol, listOf(1L, 5L))))), eval("`[1 `,(1 ,(+ 2 3))]", env))

        assertEquals(Pair(UnquoteSplicing.symbol, Symbol.intern("foo")), eval("`(unquote-splicing . foo)", env))
        assertEquals(Pair(Unquote.symbol, Pair(1L, 2L)), eval("`(unquote 1 . 2)", env))

        assertEquals(emptyList<Nothing>(), eval("`()", env))
        assertEquals(MutableVector(), eval("`#()", env))
        assertEquals(listOf(1L, 2L, emptyList<Nothing>()), eval("`(1 2 ())", env))
        assertEquals(listOf(1L, 2L, listOf(Quote.symbol, emptyList<Nothing>())), eval("`(1 2 '())", env))

        try {
            eval("unquote", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("unquote: bad syntax in form: unquote", e.message)
        }
        try {
            eval("(quasiquote (unquote 1 2))", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("unquote: bad syntax (unquote expects exactly one expression) in form: (unquote 1 2)", e.message)
        }
        try {
            eval("`[1 unquote 2]", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("read: illegal use of '.'", e.message)
        }
    }

    @Test
    fun testDuplicateArgumentsAreNotAllowed() {
        try {
            eval("(lambda (a a) a)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax (duplicate argument name: a) in form: (lambda (a a) a)", e.message)
        }
        try {
            eval("(define (a b b) b)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            assertEquals("lambda: bad syntax (duplicate argument name: b) in form: (lambda (b b) b)", e.message)
        }
    }

    @Test
    fun testThrow() {
        try {
            eval("(throw (new Exception))", env)
            fail()
        } catch (e: Exception) {
            // expected
        }
        try {
            eval("(throw (new NullPointerException \"BOOM\"))", env)
            fail()
        } catch (e: NullPointerException) {
            // expected
        }
        try {
            eval("(throw (new StackOverflowError))", env)
            fail()
        } catch (e: StackOverflowError) {
            // expected
        }
    }

    @Test
    fun testTryCatchFinally() {
        assertEquals(null, eval("(try)", env))
        assertEquals(null, eval("(try (catch Exception e))", env))
        assertEquals(null, eval("(try (finally))", env))
        assertEquals(null, eval("(try (catch Exception e) (finally))", env))
        assertEquals(6L, eval("(try 6)", env))
        assertEquals(6L, eval("(try (+ 1 2 3) (finally))", env))
        assertEquals(3L, eval("(try 1 2 3 (catch Exception e) (finally))", env))
        assertEquals(3L, eval("(let ((a 0)) (try (set! a (inc a)) (throw (new Exception)) (catch Exception e (set! a (inc a))) (finally (set! a (inc a)))) a)", env))
        assertEquals(2L, eval("(let ((a 0)) (try (set! a (inc a)) (throw (new Exception)) (catch Exception e (set! a (inc a)))) a)", env))
        assertEquals(2L, eval("(let ((a 0)) (try (set! a (inc a)) (finally (set! a (inc a)))) a)", env))
        assertEquals(6L, eval("(let ((a 0)) (try (set! a 5) (finally (set! a (inc a)))) a)", env))
        assertEquals(Exception::class.java, eval("(class (try (throw (new Exception)) (catch Exception e e)))", env))
        val illegalSyntax = arrayOf("(try 1 2 (finally) 3 (catch Exception e) (finally))",
                                    "(try 1 2 3 (catch Exception e) (finally) (catch Exception e))",
                                    "(try 1 (catch Exception e) 2 3)",
                                    "(try 1 (finally 1) 2 3)",
                                    "(try 1 (catch Exception))",
                                    "(try 1 (catch))",
                                    "(try 1 (catch 1))",
                                    "(try 1 (catch a))",
                                    "(try 1 (catch Exception 123))")
        for (illegal in illegalSyntax) {
            try {
                eval(illegal, env)
                fail()
            } catch (e: IllegalSyntaxException) {
                // success
            }
        }
    }

    @Test
    fun testNilValue() {
        assertEquals(null, eval("(let ((a nil)) a)", env))
        assertEquals(null, eval("(let* ((a nil)) a)", env))
        assertEquals(null, eval("(letrec ((a nil)) a)", env))
        assertEquals(null, eval("(begin (define nv nil) nv)", env))
        assertEquals(null, eval("(begin (define nv 5) (set! nv nil) nv)", env))
        try {
            eval("some-undefined-identifier", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
        assertEquals(null, eval("(begin (define some-undefined-identifier nil) some-undefined-identifier)", env))
    }

    @Test
    fun testInstanceOf() {
        assertEquals(true, eval("(instance? String \"str\")", env))
        assertEquals(true, eval("(instance? Object \"str\")", env))
        assertEquals(true, eval("(instance? Object (new NullPointerException))", env))
        assertEquals(true, eval("(instance? Class (.getClass 1))", env))
        assertEquals(true, eval("(instance? (.getClass []) [])", env))
        assertEquals(true, eval("(instance? java.math.BigDecimal (new java.math.BigDecimal 10))", env))
        assertEquals(false, eval("(instance? String 1)", env))
        assertEquals(false, eval("(instance? String [])", env))
        assertEquals(false, eval("(instance? String (new Object))", env))
        assertEquals(false, eval("(instance? Number \"\")", env))
        assertEquals(false, eval("(instance? (.getClass {}) #{})", env))
    }

    @Test
    fun testYCombinator() {
        val Y = "(define Y" +
                "  (lambda (h)" +
                "    ((lambda (x) (x x))" +
                "     (lambda (g)" +
                "       (h (lambda args (apply (g g) args)))))))"
        eval(Y, env)

        val fac = "(define fac" +
                "  (Y" +
                "    (lambda (f)" +
                "      (lambda (x)" +
                "        (if (< x 2)" +
                "            1" +
                "            (* x (f (- x 1))))))))"

        val fib = "(define fib" +
                "  (Y" +
                "    (lambda (f)" +
                "      (lambda (x)" +
                "        (if (< x 2)" +
                "            x" +
                "            (+ (f (- x 1)) (f (- x 2))))))))"
        eval(fac, env)
        eval(fib, env)
        assertEquals(720L, eval("(fac 6)", env))
        assertEquals(8L, eval("(fib 6)", env))
    }

    @Test
    fun testThunkForm() {
        assertTrue(eval("(procedure? (thunk 1 2 3))", env) as Boolean)
        assertEquals(3L, eval("((thunk 1 2 3))", env))
        assertEquals(6L, eval("((thunk (+ 1 2 3)))", env))
        assertEquals(1L, eval("((thunk (define x 1) x)))", env))
        assertEquals(null, eval("((thunk nil)))", env))
        try {
            eval("(thunk)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
    }

    @Test
    fun testCallCCForm() {
        assertTrue(eval("(call/cc identity)", env) is Continuation)
        try {
            eval("(call/cc)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
        try {
            eval("(call/cc 1 2)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
        try {
            eval("(call/cc 1)", env)
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testQuoteForm() {
        assertTrue(eval("(quote identity)", env) is AFn<*, *>)
        try {
            eval("(quote)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
        try {
            eval("(quote 1 2)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
    }
}

