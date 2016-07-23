package unittests.s7.tests;

import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static junit.framework.TestCase.assertEquals;

public class EqvTest extends AbstractS7Test {

  @Test
  public void testEqv() {
    assertEquals(FALSE, eval("(eqv? 'a 3)", env));
    assertEquals(FALSE, eval("(eqv? #t 't)", env));
    assertEquals(FALSE, eval("(eqv? \"abs\" 'abc)", env));
    assertEquals(FALSE, eval("(eqv? \"hi\" '(hi))", env));
    assertEquals(FALSE, eval("(eqv? \"()\" '())", env));
    assertEquals(FALSE, eval("(eqv? '(1) '(1))", env));
    assertEquals(FALSE, eval("(eqv? '(#f) '(#f))", env));
    assertEquals(FALSE, eval("(eqv? #\\a #\\b)", env));
    assertEquals(TRUE, eval("(eqv? #\\a #\\a)", env));
    assertEquals(TRUE, eval("(eqv? #\\space #\\space)", env));
    assertEquals(TRUE, eval("(eqv? #\\  #\\space)", env));
    assertEquals(TRUE, eval("(eqv? #\\space #\\space)", env));
    assertEquals(TRUE, eval("(eqv? #\\newline '#\\newline)", env));
    assertEquals(TRUE, eval("(let ((x (string-ref \"hi\" 0))) (eqv? x x))", env));
    assertEquals(TRUE, eval("(eqv? #t #t)", env));
    assertEquals(TRUE, eval("(eqv? #f #f)", env));
    assertEquals(FALSE, eval("(eqv? #f #t)", env));
    assertEquals(TRUE, eval("(eqv? (null? '()) #t)", env));
    assertEquals(TRUE, eval("(eqv? (null? '(a)) #f)", env));
    assertEquals(TRUE, eval("(eqv? (cdr '(a)) '())", env));
    assertEquals(TRUE, eval("(eqv? 'a 'a)", env));
    assertEquals(FALSE, eval("(eqv? 'a 'b)", env));
    assertEquals(FALSE, eval("(eqv? '(a) '(b))", env));
    assertEquals(TRUE, eval("(let ((x '(a . b))) (eqv? x x))", env));
    assertEquals(TRUE, eval("(let ((x (cons 'a 'b))) (eqv? x x))", env));
    assertEquals(FALSE, eval("(eqv? (cons 'a 'b) (cons 'a 'b))", env));
    assertEquals(FALSE, eval("(eqv? \"abc\" \"cba\")", env));
    assertEquals(TRUE, eval("(let ((x \"hi\")) (eqv? x x))", env));
    assertEquals(FALSE, eval("(eqv? (string #\\h #\\i) (string #\\h #\\i))", env));
    assertEquals(FALSE, eval("(eqv? #(a) #(b))", env));
    assertEquals(TRUE, eval("(let ((x (vector 'a))) (eqv? x x))", env));
    assertEquals(FALSE, eval("(eqv? (vector 'a) (vector 'a))", env));
    assertEquals(TRUE, eval("(eqv? car car)", env));
    assertEquals(FALSE, eval("(eqv? car cdr)", env));
    assertEquals(TRUE, eval("(let ((x (lambda () 1))) (eqv? x x))", env));
    assertEquals(FALSE, eval("(eqv? (lambda () 1) (lambda () 1))", env));
    assertEquals(FALSE, eval("(let () (define (make-adder x) (lambda (y) (+ x y))) (eqv? (make-adder 1) (make-adder 1)))", env));
    assertEquals(TRUE, eval("(eqv? 'quote 'quote) #t)", env));
    assertEquals(FALSE, eval("(eqv? ''2 '2)", env));
    assertEquals(TRUE, eval("(eqv? '2 '2)", env));
    assertEquals(TRUE, eval("(eqv? '2 2)", env));
    assertEquals(FALSE, eval("(eqv? ''2 ''2)", env));
    assertEquals(FALSE, eval("(eqv? ''#\\a '#\\a)", env));
    assertEquals(TRUE, eval("(eqv? '#\\a #\\a)", env));
    assertEquals(FALSE, eval("(eqv? 'car car)", env));
    assertEquals(FALSE, eval("(eqv? ''() '())", env));
    assertEquals(TRUE, eval("(eqv? '#f #f)", env));
    assertEquals(TRUE, eval("(eqv? '#f '#f)", env));

//    assertEquals(TRUE, eval("(eqv? 'a (string->symbol "a"))", env));
//    assertEquals(, "(eqv? (integer->char 255) (string-ref (string #\x (integer->char 255) #\x) 1)) #t)
//    assertEquals(, "(eqv? (integer->char #xf0) (integer->char #x70)) #f)
  }
}
