package unittests.s7.tests;

import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static junit.framework.TestCase.assertEquals;

public class EqTest extends AbstractS7Test {

  @Test
  public void testEq() {
    assertEquals(FALSE, eval("(eq? 'a 3)", env));
    assertEquals(FALSE, eval("(eq? #t 't)", env));
    assertEquals(FALSE, eval("(eq? \"abs\" 'abc)", env));
    assertEquals(FALSE, eval("(eq? \"hi\" '(hi))", env));
    assertEquals(FALSE, eval("(eq? \"hi\" \"hi\")", env));
    assertEquals(FALSE, eval("(eq? \"()\" '())", env));
    assertEquals(FALSE, eval("(eq? '(1) '(1))", env));
    assertEquals(FALSE, eval("(eq? '(#f) '(#f))", env));
    assertEquals(FALSE, eval("(eq? #\\a #\\b)", env));
    assertEquals(TRUE, eval("(eq? #t #t)", env));
    assertEquals(TRUE, eval("(eq? #f #f)", env));
    assertEquals(FALSE, eval("(eq? #f #t)", env));
    assertEquals(TRUE, eval("(eq? (null? '()) #t)", env));
    assertEquals(TRUE, eval("(eq? (null? '(a)) #f)", env));
    assertEquals(TRUE, eval("(eq? (cdr '(a)) '())", env));
    assertEquals(TRUE, eval("(eq? 'a 'a)", env));
    assertEquals(FALSE, eval("(eq? 'a 'b)", env));
    assertEquals(TRUE, eval("(eq? 'a (string->symbol \"a\"))", env));
    assertEquals(TRUE, eval("(let ((x '(a . b))) (eq? x x))", env));
    assertEquals(TRUE, eval("(let ((x (cons 'a 'b))) (eq? x x))", env));
    assertEquals(FALSE, eval("(eq? (cons 'a 'b) (cons 'a 'b))", env));
    assertEquals(FALSE, eval("(eq? \"abc\" \"cba\")", env));
    assertEquals(TRUE, eval("(let ((x \"hi\")) (eq? x x))", env));
    assertEquals(FALSE, eval("(eq? (string #\\h #\\i) (string #\\h #\\i))", env));
    assertEquals(FALSE, eval("(eq? #(a) #(b))", env));
    assertEquals(TRUE, eval("(let ((x (vector 'a))) (eq? x x))", env));
    assertEquals(FALSE,eval("(eq? (vector 'a) (vector 'a))", env));
    assertEquals(TRUE, eval("(eq? car car)", env));
    assertEquals(FALSE, eval("(eq? car cdr)", env));
    assertEquals(TRUE, eval("(let ((x (lambda () 1))) (eq? x x))", env));
    assertEquals(TRUE, eval("(let ((x (lambda () 1))) (let ((y x)) (eq? x y)))", env));
    assertEquals(FALSE, eval("(let ((x (lambda () 1))) (let ((y (lambda () 1))) (eq? x y)))", env));
    assertEquals(TRUE, eval("(eq? 'abc 'abc)", env));
    assertEquals(TRUE, eval("(eq? eq? eq?)", env));
    assertEquals(FALSE, eval("(eq? (if #f 1) 1)", env));
    assertEquals(TRUE, eval("(eq? '() '())", env));
    assertEquals(TRUE, eval("(eq? '() '(  ))", env));
    assertEquals(TRUE, eval("(eq? '()'())", env));
    assertEquals(TRUE, eval("(eq? '() (list))", env));
    assertEquals(TRUE, eval("(eq? '() (list))", env));
    assertEquals(FALSE, eval("(eq? ''#\\a '#\\a)", env));
    assertEquals(FALSE, eval("(eq? 'car car)", env));
    assertEquals(FALSE, eval("(eq? ''() '())", env));
    assertEquals(TRUE, eval("(eq? '#f #f)", env));
    assertEquals(TRUE, eval("(eq? '#f '#f)", env));
    assertEquals(TRUE, eval("(eq? #f '  #f)", env));
    assertEquals(TRUE, eval("(eq? '()'())", env));
    assertEquals(FALSE, eval("(let ((f (lambda () (cons 1 (string #\\H))))) (eq? (f) (f)))", env));
    assertEquals(TRUE, eval("(eq? 'if 'if)", env));
    assertEquals(FALSE, eval("(eq? (string) \"\")", env));
    assertEquals(FALSE, eval("(eq? (vector) (vector))", env));
    assertEquals(FALSE, eval("(eq? (vector) #())", env));
    assertEquals(TRUE, eval("(eq? (list) (list))", env));
    assertEquals(TRUE, eval("(eq? (list) '())", env));

    eval("(define (counter count)" +
         "  (lambda ()" +
         "    (set! count (+ 1 count))" +
         "    count))", env);
    eval("(define c1 (counter 0))", env);
    eval("(define c2 (counter 0))", env);
    assertEquals(FALSE, eval("(eq? c1 c2)", env));
    assertEquals(TRUE,  eval("(eq? c1 c1)", env));
    assertEquals(TRUE,  eval("(eq? c2 c2)", env));

//    assertEquals(TRUE, eval("(let ((f (lambda () (quote (1 . \"H\"))))) (eq? (f) (f)))", env));
//    assertEquals(FALSE, eval("(eq? '#\\a #\\a)", env));
//    assertEquals(FALSE, eval("(eq? (string) (string))", env));
//    assertEquals(TRUE, eval("(eq? (symbol \"a\") (string->symbol \"a\"))", env));
  }
}
