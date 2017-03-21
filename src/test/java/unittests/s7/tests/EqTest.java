package unittests.s7.tests;

import org.junit.Test;
import unittests.AbstractTest;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static junit.framework.TestCase.assertEquals;

public class EqTest extends AbstractTest {

  @Test
  public void testEq() {
    String[] trues = {
        "(eq? \"hi\" \"hi\")", "(eq? #t #t)", "(eq? #f #f)", "(eq? (null? '()) #t)", "(eq? (null? '(a)) #f)",
        "(eq? (cdr '(a)) '())", "(eq? 'a 'a)", "(eq? 'a (string->symbol \"a\"))", "(let ((x '(a . b))) (eq? x x))",
        "(let ((x (cons 'a 'b))) (eq? x x))", "(let ((x \"hi\")) (eq? x x))", "(let ((x (vector 'a))) (eq? x x))",
        "(eq? car car)", "(let ((x (lambda () 1))) (eq? x x))", "(let ((x (lambda () 1))) (let ((y x)) (eq? x y)))",
        "(eq? 'abc 'abc)", "(eq? eq? eq?)", "(eq? '() '())", "(eq? '() '(  ))", "(eq? '()'())", "(eq? '() (list))",
        "(eq? '() (list))", "(eq? '#f #f)", "(eq? '#f '#f)", "(eq? #f '  #f)", "(eq? '()'())", "(eq? 'if 'if)",
        "(eq? (list) (list))", "(eq? (list) '())", };
    assertAllEqual(TRUE, trues, env);

    String[] falses = {
        "(let ((x (lambda () 1))) (let ((y (lambda () 1))) (eq? x y)))", "(eq? (if #f 1) 1)",
        "(eq? ''#\\a '#\\a)", "(eq? 'car car)", "(eq? ''() '())", "(eq? (string) \"\")",
        "(let ((f (lambda () (cons 1 (string #\\H))))) (eq? (f) (f)))", "(eq? (vector) (vector))",
        "(eq? (vector) #())", "(eq? 'a 3)", "(eq? #t 't)", "(eq? \"abs\" 'abc)", "(eq? \"hi\" '(hi))",
        "(eq? \"()\" '())", "(eq? '(1) '(1))", "(eq? '(#f) '(#f))", "(eq? #\\a #\\b)", "(eq? #f #t)",
        "(eq? 'a 'b)", "(eq? (cons 'a 'b) (cons 'a 'b))", "(eq? \"abc\" \"cba\")",
        "(eq? (string #\\h #\\i) (string #\\h #\\i))", "(eq? #(a) #(b))", "(eq? (vector 'a) (vector 'a))",
        "(eq? car cdr)", };
    assertAllEqual(FALSE, falses, env);

    eval("(define (counter count)" +
         "  (lambda ()" +
         "    (set! count (+ 1 count))" +
         "    count))", env);
    eval("(define c1 (counter 0))", env);
    eval("(define c2 (counter 0))", env);
    assertEquals(FALSE, eval("(eq? c1 c2)", env));
    assertEquals(TRUE,  eval("(eq? c1 c1)", env));
    assertEquals(TRUE,  eval("(eq? c2 c2)", env));
  }
}
