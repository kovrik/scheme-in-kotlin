package s7.tests;

import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static junit.framework.TestCase.assertEquals;

public class EqualTest extends AbstractS7Test {

  @Test
  public void testEqual() {
    assertEquals(FALSE, eval("(equal? 'a 3)", env));
    assertEquals(FALSE, eval("(equal? #t 't)", env));
    assertEquals(FALSE, eval("(equal? \"abs\" 'abc)", env));
    assertEquals(FALSE, eval("(equal? \"hi\" '(hi))", env));
    assertEquals(FALSE, eval("(equal? \"()\" '())", env));
    assertEquals(TRUE, eval("(equal? '(1) '(1))", env));
    assertEquals(TRUE, eval("(equal? '(#f) '(#f))", env));
    assertEquals(TRUE, eval("(equal? '(()) '(() . ()))", env));
    assertEquals(FALSE, eval("(equal? #\\a #\\b)", env));
    assertEquals(TRUE, eval("(equal? #\\a #\\a)", env));
    assertEquals(TRUE, eval("(let ((x (string-ref \"hi\" 0))) (equal? x x))", env));
    assertEquals(TRUE, eval("(equal? #t #t)", env));
    assertEquals(TRUE, eval("(equal? #f #f)", env));
    assertEquals(FALSE, eval("(equal? #f #t)", env));
    assertEquals(TRUE, eval("(equal? (null? '()) #t)", env));
    assertEquals(TRUE, eval("(equal? (null? '(a)) #f)", env));
    assertEquals(TRUE, eval("(equal? (cdr '(a)) '())", env));
    assertEquals(TRUE, eval("(equal? 'a 'a)", env));
    assertEquals(FALSE, eval("(equal? 'a 'b)", env));
    assertEquals(FALSE, eval("(equal? '(a) '(b))", env));
    assertEquals(TRUE, eval("(equal? '(a) '(a))", env));
    assertEquals(TRUE, eval("(let ((x '(a . b))) (equal? x x))", env));
    assertEquals(TRUE, eval("(let ((x (cons 'a 'b))) (equal? x x))", env));
    assertEquals(TRUE, eval("(equal? (cons 'a 'b) (cons 'a 'b))", env));
    assertEquals(TRUE, eval("(equal?(cons 'a 'b)(cons 'a 'b))", env));
    assertEquals(FALSE, eval("(equal? \"abc\" \"cba\")", env));
    assertEquals(TRUE, eval("(equal? \"abc\" \"abc\")", env));
    assertEquals(TRUE, eval("(let ((x \"hi\")) (equal? x x))", env));
    assertEquals(TRUE, eval("(equal? (string #\\h #\\i) (string #\\h #\\i))", env));
    assertEquals(FALSE, eval("(equal? #(a) #(b))", env));
    assertEquals(TRUE, eval("(equal? #(a) #(a))", env));
    assertEquals(TRUE, eval("(let ((x (vector 'a))) (equal? x x))", env));
    assertEquals(TRUE, eval("(equal? (vector 'a) (vector 'a))", env));
    assertEquals(TRUE, eval("(equal? #(1 2) (vector 1 2))", env));
    assertEquals(FALSE, eval("(equal? #(1 2) (vector 1 2.0))", env));
    assertEquals(TRUE, eval("(equal? '(1 . 2) (cons 1 2))", env));
    assertEquals(TRUE, eval("(equal? #(1 \"hi\" #\\a) (vector 1 \"hi\" #\\a))", env));
    assertEquals(TRUE, eval("(equal? #((1 . 2)) (vector (cons 1 2)))", env));
    assertEquals(TRUE, eval("(equal? #(1 \"hi\" #\\a (1 . 2)) (vector 1 \"hi\" #\\a (cons 1 2)))", env));
    assertEquals(TRUE, eval("(equal? #(#f hi (1 2) 1 \"hi\" #\\a (1 . 2)) (vector #f 'hi (list 1 2) 1 \"hi\" #\\a (cons 1 2)))", env));
    assertEquals(TRUE, eval("(equal? #(#(1) #(1)) (vector (vector 1) (vector 1)))", env));
    assertEquals(TRUE, eval("(equal? #(()) (vector '()))", env));
    assertEquals(TRUE, eval("(equal? #(\"hi\" \"ho\") (vector \"hi\" '\"ho\"))", env));
    assertEquals(FALSE, eval("(equal? ''#(1) #(1))", env));
    assertEquals(FALSE, eval("(equal? ''#(1) '#(1))", env));
    assertEquals(TRUE, eval("(equal? '(1) '        (   1    ))", env));
    assertEquals(TRUE, eval("(equal? '((())) (list (list (list))))", env));
    assertEquals(TRUE, eval("(equal? '((())) (cons (cons '() '()) '()))", env));
    assertEquals(TRUE, eval("(equal? car car)", env));
    assertEquals(FALSE, eval("(equal? car cdr)", env));
    assertEquals(TRUE, eval("(let ((x (lambda () 1))) (equal? x x))", env));
    assertEquals(FALSE, eval("(equal? (lambda () 1) (lambda () 1))", env));
    assertEquals(TRUE, eval("(equal? #((())) #((())))", env));
    assertEquals(TRUE, eval("(equal? #()#())", env));
    assertEquals(FALSE, eval("(equal? #()'())", env));
    assertEquals(FALSE, eval("(equal? '()\"\")", env));
    assertEquals(TRUE, eval("(equal? \"hi\"\"hi\")", env));
    assertEquals(TRUE, eval("(let () (define (hi a) (+ 1 a)) (equal? hi hi))", env));
    assertEquals(TRUE, eval("(equal? (list 'abs 'cons) '(abs cons))", env));
    assertEquals(FALSE, eval("(equal? '(1) '(list 1))", env));
    assertEquals(FALSE, eval("(equal? 9223372036854775807 9223372036854775806)", env));
    assertEquals(FALSE, eval("(equal? 9223372036854775807 -9223372036854775808)", env));
    assertEquals(TRUE, eval("(equal? -9223372036854775808 -9223372036854775808)", env));
    assertEquals(TRUE, eval("(let ((x 3.141)) (equal? x x))", env));
    assertEquals(TRUE, eval("(equal? 3 3)", env));
    assertEquals(FALSE, eval("(equal? 3 3.0)", env));
    assertEquals(TRUE, eval("(equal? 3.0 3.0)", env));
    assertEquals(TRUE, eval("(equal? (string #\\c) \"c\")", env));
    assertEquals(TRUE, eval("(equal? equal? equal?)", env));
    assertEquals(TRUE, eval("(equal? (cons 1 (cons 2 3)) '(1 2 . 3))", env));
    assertEquals(TRUE, eval("(equal? '() '())", env));
    assertEquals(TRUE, eval("(equal? '() (list))", env));
    assertEquals(TRUE, eval("(equal? (cdr '   ''0) '((quote 0)))", env));
    assertEquals(TRUE, eval("(equal? \"\\n\" \"\\n\")", env));
    assertEquals(TRUE, eval("(equal? #f ((lambda () #f)))", env));
    assertEquals(TRUE, eval("(equal? (+) 0)", env));
    assertEquals(TRUE, eval("(equal? \"asd\"\"asd\")", env));
    assertEquals(TRUE, eval("(let hiho ((i 0)) (equal? hiho hiho))", env));
    assertEquals(FALSE, eval("(let hiho ((i 0)) (let hoho ((i 0)) (equal? hiho hoho)))", env));
    assertEquals(FALSE, eval("(equal? + *)", env));
    assertEquals(TRUE, eval("(equal? lambda lambda)", env));
    assertEquals(TRUE, eval("(equal? let let)", env));
    assertEquals(FALSE, eval("(equal? let letrec)", env));
    assertEquals(TRUE, eval("(equal? define define)", env));
    assertEquals(TRUE, eval("(equal? + ((lambda (a) a) +))", env));
    assertEquals(TRUE, eval("(let ((x \"hi\")) (define (hi) x) (equal? (hi) (hi)))", env));

//    (test (equal? 'a (string->symbol "a")) TRUE
//    (test (equal? 3-4i 3-4i) TRUE
  }
}
