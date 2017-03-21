package unittests.s7.tests;

import org.junit.Test;
import unittests.AbstractTest;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

public class EqvTest extends AbstractTest {

  @Test
  public void testEqv() {
    String[] trues = {"(let ((x (vector 'a))) (eqv? x x))", "(eqv? car car)", "(let ((x (lambda () 1))) (eqv? x x))",
        "(eqv? 'quote 'quote) #t)", "(eqv? '2 '2)", "(eqv? '2 2)", "(eqv? '#\\a #\\a)",
        "(eqv? '#f #f)", "(eqv? '#f '#f)", "(eqv? #\\a #\\a)", "(eqv? #\\space #\\space)",
        "(eqv? #\\  #\\space)", "(eqv? #\\space #\\space)", "(eqv? #\\newline '#\\newline)",
        "(let ((x (string-ref \"hi\" 0))) (eqv? x x))", "(eqv? #t #t)", "(eqv? #f #f)", "(eqv? (null? '()) #t)",
        "(eqv? (null? '(a)) #f)", "(eqv? (cdr '(a)) '())", "(eqv? 'a 'a)", "(let ((x '(a . b))) (eqv? x x))",
        "(let ((x (cons 'a 'b))) (eqv? x x))", "(let ((x \"hi\")) (eqv? x x))",
    };
    assertAllEqual(TRUE, trues, env);

    String[] falses = {"(eqv? 'a 3)", "(eqv? #t 't)", "(eqv? \"abs\" 'abc)", "(eqv? \"hi\" '(hi))",
        "(eqv? \"()\" '())", "(eqv? '(1) '(1))", "(eqv? '(#f) '(#f))", "(eqv? #\\a #\\b)",
        "(eqv? #f #t)", "(eqv? 'a 'b)", "(eqv? '(a) '(b))", "(eqv? (cons 'a 'b) (cons 'a 'b))", "(eqv? \"abc\" \"cba\")",
        "(eqv? (string #\\h #\\i) (string #\\h #\\i))", "(eqv? #(a) #(b))", "(eqv? (vector 'a) (vector 'a))",
        "(eqv? car cdr)", "(eqv? ''2 '2)", "(eqv? ''2 ''2)", "(eqv? ''#\\a '#\\a)", "(eqv? 'car car)",
        "(eqv? ''() '())", "(eqv? (lambda () 1) (lambda () 1))",
        "(let () (define (make-adder x) (lambda (y) (+ x y))) (eqv? (make-adder 1) (make-adder 1)))",
    };
    assertAllEqual(FALSE, falses, env);
  }
}
