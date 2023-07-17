package unittests.s7.tests

import org.junit.Test
import unittests.AbstractTest

import org.junit.Assert.assertEquals

class EqTest : AbstractTest() {

    @Test
    fun testEq() {
        val trues = arrayOf(
                "(eq? \"hi\" \"hi\")", "(eq? #t #t)", "(eq? #f #f)", "(eq? (null? null) #t)", "(eq? (null? '(a)) #f)",
                "(eq? (cdr '(a)) '())", "(eq? 'a 'a)", "(eq? 'a (string->symbol \"a\"))", "(let ((x '(a . b))) (eq? x x))",
                "(let ((x (cons 'a 'b))) (eq? x x))", "(let ((x \"hi\")) (eq? x x))", "(let ((x (vector 'a))) (eq? x x))",
                "(eq? car car)", "(let ((x (lambda () 1))) (eq? x x))", "(let ((x (lambda () 1))) (let ((y x)) (eq? x y)))",
                "(eq? 'abc 'abc)", "(eq? eq? eq?)", "(eq? '() '())", "(eq? '() '(  ))", "(eq? '()'())", "(eq? '() (list))",
                "(eq? '() (list))", "(eq? '#f #f)", "(eq? '#f '#f)", "(eq? #f '  #f)", "(eq? '()'())", "(eq? 'if 'if)",
                "(eq? (list) (list))", "(eq? (list) '())", "(eq? :test :test)", "(eq? :test (keyword \"test\"))",
                "(eq? '+ '+)", "(eq? 'test (symbol \"test\"))")
        assertAllEqual(true, trues)

        val falses = arrayOf(
                "(let ((x (lambda () 1))) (let ((y (lambda () 1))) (eq? x y)))", "(eq? (when #f 1) 1)", "(eq? ''#\\a '#\\a)",
                "(eq? 'car car)", "(eq? ''() '())", "(eq? (string) \"\")",
                "(let ((f (lambda () (cons 1 (string #\\H))))) (eq? (f) (f)))", "(eq? (vector) (vector))", "(eq? (vector) #())",
                "(eq? 'a 3)", "(eq? #t 't)", "(eq? \"abs\" 'abc)", "(eq? \"hi\" '(hi))", "(eq? \"()\" '())",
                "(eq? '(1) '(1))", "(eq? '(#f) '(#f))", "(eq? #\\a #\\b)", "(eq? #f #t)", "(eq? 'a 'b)", "(eq? (cons 'a 'b) (cons 'a 'b))",
                "(eq? \"abc\" \"cba\")", "(eq? (string #\\h #\\i) (string #\\h #\\i))", "(eq? '#(a) '#(b))",
                "(eq? (vector 'a) (vector 'a))", "(eq? car cdr)")
        assertAllEqual(false, falses)

        eval("(define (counter count)" +
                "  (lambda ()" +
                "    (set! count (+ 1 count))" +
                "    count))")
        eval("(define c1 (counter 0))")
        eval("(define c2 (counter 0))")
        assertEquals(false, eval("(eq? c1 c2)"))
        assertEquals(true, eval("(eq? c1 c1)"))
        assertEquals(true, eval("(eq? c2 c2)"))

        eval("(define sym1 'test")
        eval("(define sym2 'test")
        eval("(define sym3 (with-meta sym2 {:test #t}))")
        assertEquals(true, eval("(eq? sym1 sym2 sym3)"))
        assertEquals(false, eval("(identical? sym1 sym2 sym3)"))
    }
}
