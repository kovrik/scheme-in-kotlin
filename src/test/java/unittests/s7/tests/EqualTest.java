package unittests.s7.tests;

import org.junit.Test;
import unittests.AbstractTest;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

public class EqualTest extends AbstractTest {

  @Test
  public void testEqual() {

    String[] trues = {"(equal? #(a) #(a))", "(equal? '(1) '(1))", "(equal? '(#f) '(#f))", "(equal? '(()) '(() . ()))",
        "(equal? #\\a #\\a)", "(let ((x (string-ref \"hi\" 0))) (equal? x x))", "(equal? #t #t)",
        "(equal? #f #f)", "(equal? (null? null) #t)", "(equal? (null? '(a)) #f)",
        "(equal? (cdr '(a)) '())", "(equal? 'a 'a)", "(equal? '(a) '(a))", "(let ((x '(a . b))) (equal? x x))",
        "(let ((x (cons 'a 'b))) (equal? x x))", "(equal? (cons 'a 'b) (cons 'a 'b))",
        "(equal?(cons 'a 'b)(cons 'a 'b))", "(equal? \"abc\" \"abc\")",
        "(equal? \"abc\" (string->immutable-string \"abc\"))", "(let ((x \"hi\")) (equal? x x))",
        "(equal? (string #\\h #\\i) (string #\\h #\\i))", "(equal? #(a) (vector->immutable-vector #(a)))",
        "(let ((x (vector 'a))) (equal? x x))", "(equal? (vector 'a) (vector 'a))", "(equal? #(1 2) (vector 1 2))",
        "(equal? '(1 . 2) (cons 1 2))", "(equal? #(1 \"hi\" #\\a) (vector 1 \"hi\" #\\a))",
        "(equal? #((1 . 2)) (vector (cons 1 2)))", "(equal? #(1 \"hi\" #\\a (1 . 2)) (vector 1 \"hi\" #\\a (cons 1 2)))",
        "(equal? #(#f hi (1 2) 1 \"hi\" #\\a (1 . 2)) (vector #f 'hi (list 1 2) 1 \"hi\" #\\a (cons 1 2)))",
        "(equal? #([1] [1]) (vector (vector 1) (vector 1)))", "(equal? #(()) (vector '()))",
        "(equal? #(\"hi\" \"ho\") (vector \"hi\" '\"ho\"))", "(equal? '(1) '        (   1    ))",
        "(equal? '((())) (list (list (list))))", "(equal? '((())) (cons (cons '() '()) '()))", "(equal? car car)",
        "(let ((x (lambda () 1))) (equal? x x))", "(equal? #((())) #((())))", "(equal? #()#())", "(equal? \"hi\"\"hi\")",
        "(let () (define (hi a) (+ 1 a)) (equal? hi hi))", "(equal? (list 'abs 'cons) '(abs cons))",
        "(equal? -9223372036854775808 -9223372036854775808)", "(let ((x 3.141)) (equal? x x))", "(equal? 3 3)",
        "(equal? 3.0 3.0)", "(equal? (string #\\c) \"c\")", "(equal? equal? equal?)",
        "(equal? (cons 1 (cons 2 3)) '(1 2 . 3))", "(equal? '() '())", "(equal? '() (list))",
        "(equal? (cdr '   ''0) '((quote 0)))", "(equal? \"\\n\" \"\\n\")", "(equal? #f ((lambda () #f)))",
        "(equal? (+) 0)", "(equal? \"asd\"\"asd\")", "(let hiho ((i 0)) (equal? hiho hiho))", "(equal? 'lambda 'lambda)",
        "(equal? 'let 'let)", "(equal? 'define 'define)", "(equal? + ((lambda (a) a) +))",
        "(let ((x \"hi\")) (define (hi) x) (equal? (hi) (hi)))", "(equal? 'a (string->symbol \"a\"))", "(equal? 3-4i 3-4i))",
        "(equal? 0+0i 0+0i))", "(equal? 3/4 3/4))", "(equal? 30/40 3/4))",};

    assertAllEqual(TRUE, trues, env);

    String[] falses = {"(equal? 1+3i 2+4i))", "(equal? 31/40 3/4))", "(equal? 'a 3)", "(equal? #t 't)",
        "(equal? \"abs\" 'abc)", "(equal? \"hi\" '(hi))", "(equal? \"()\" '())", "(equal? #\\a #\\b)",
        "(equal? #f #t)", "(equal? 'a 'b)", "(equal? '(a) '(b))", "(equal? \"abc\" \"cba\")", "(equal? #(a) #(b))",
        "(equal? #(1 2) (vector 1 2.0))", "(equal? #(1) #('1))", "(equal? car cdr)",
        "(equal? (lambda () 1) (lambda () 1))", "(equal? '(1) '(list 1))",
        "(equal? 9223372036854775807 9223372036854775806)", "(equal? 9223372036854775807 -9223372036854775808)",
        "(equal? 3 3.0)", "(let hiho ((i 0)) (let hoho ((i 0)) (equal? hiho hoho)))", "(equal? + *)",
        "(equal? 'let 'letrec)", "(equal? #()'())", "(equal? '()\"\")",
    };
    assertAllEqual(FALSE, falses, env);
  }
}
