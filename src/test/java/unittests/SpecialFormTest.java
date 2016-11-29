package unittests;

import core.Main;
import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.WrongTypeException;
import core.procedures.io.Display;
import core.scm.*;
import core.scm.specialforms.Quote;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.*;
import static core.scm.SCMUnspecified.UNSPECIFIED;
import static org.junit.Assert.*;

public class SpecialFormTest extends AbstractTest {

  @Test
  public void testEvalImplicitBegin() {
    assertEquals(3L, eval("((lambda () 1 2 (+ 1 2)))", env));
    assertEquals(3L, eval("(let    () 1 2 (+ 1 2))", env));
    assertEquals(3L, eval("(let*   () 1 2 (+ 1 2))", env));
    assertEquals(3L, eval("(letrec () 1 2 (+ 1 2))", env));

    eval("(define (a) 1 2 (+ 1 2))", env);
    assertEquals(3L, eval("(a)", env));
    // TODO do
    // TODO named-lambda
    // TODO fluid-let
  }

  @Test
  public void testEvalMutualRecursion() {
    String f = "(define (F n) (if (= n 0) 1 (- n (M (F (- n 1))))))";
    String m = "(define (M n) (if (= n 0) 0 (- n (F (M (- n 1))))))";
    eval(f, env);
    eval(m, env);

    long[] fs = {1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13};
    for (int i = 0; i < fs.length; i++) {
      assertEquals(fs[i], eval(String.format("(F %s)", i), env));
    }

    long[] ms = {0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12};
    for (int i = 0; i < ms.length; i++) {
      assertEquals(ms[i], eval(String.format("(M %s)", i), env));
    }

    String letrec = "(letrec ((F (lambda (n) (if (= n 0) 1 (- n (M (F (- n 1)))))))" +
        "(M (lambda (n) (if (= n 0) 0 (- n (F (M (- n 1))))))))" +
        "(F 19))";
    assertEquals(12L, eval(letrec, env));
  }

  @Test
  public void testEvalDelayed() {
    assertEquals(1d, eval("(force (delay 1.0))", env));
    assertEquals(new SCMString("test"), eval("(force (delay \"test\"))", env));
    assertEquals(10L, eval("(force (delay (+ 5 2 (* 1 3))))", env));
    assertEquals(SCMPromise.class, eval("(delay 1.0)", env).getClass());
    assertEquals(TRUE, eval("(promise? (delay 1.0))", env));
    assertEquals(3L, eval("(force (delay (+ 1 2)))", env));
    assertEquals(list(3L, 3L), eval("(let ((p (delay (+ 1 2))))(list (force p) (force p)))", env));

    eval("(define perr (delay (error \"BOOM\")))", env);
    try {
      eval("(force perr)", env);
      fail();
    } catch (SCMError e) {
      assertEquals("BOOM", e.getMessage());
    }
    try {
      eval("(force perr)", env);
      fail();
    } catch (SCMError e) {
      assertEquals("BOOM", e.getMessage());
    }
  }

  @Test
  public void testEvalProcedure() {
    assertEquals(SCMProcedure.class, eval("(lambda () #t)", env).getClass());
    assertEquals(TRUE, eval("((lambda () #t))", env));
    assertEquals(6L, eval("((lambda (n) (+ n 1)) 5)", env));

    eval("(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))", env);
    assertEquals(8L, eval("(fib 5)", env));

    assertEquals(6L, eval("((lambda (n) (+ n 1)) 5)", env));

    // rest arguments
    assertEquals(list(1L, 2L, 3L), eval("((lambda x x) 1 2 3)", env));
    assertEquals(NIL, eval("((lambda x x))", env));
    assertEquals(1L, eval("((lambda x (car x)) 1 2 3)", env));
    assertEquals(1L, eval("((lambda (f s . rs) f) 1 2 3 4)", env));
    assertEquals(2L, eval("((lambda (f s . rs) s) 1 2 3 4)", env));
    assertEquals(list(3L, 4L), eval("((lambda (f s . rs) rs) 1 2 3 4)", env));
  }

  @Test
  public void testEvalDefine() {
    eval("(define a 5)", env);
    assertEquals(5L, eval("a", env));
    assertEquals(UNSPECIFIED, eval("(define b 7)", env));

    eval("(define edl (lambda (n) (+ n 1)))", env);
    assertEquals(2L, eval("(edl 1)", env));

    // variadic
    eval("(define edlv (lambda args args))", env);
    assertEquals(SCMCons.list(1L, 2L, 3L, 4L, 5L), eval("(edlv 1 2 3 4 5)", env));

    // variadic define
    eval("(define (edv1 first second . rest) rest)", env);
    assertEquals(SCMCons.list(2L, 3L, 4L, 5L), eval("(edv1 0 1 2 3 4 5)", env));

    eval("(define (edv2 first second . rest) second)", env);
    assertEquals(1L, eval("(edv2 0 1 2 3 4 5)", env));

    try {
      eval("(define)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("define: bad syntax in form: (define)", e.getMessage());
    }
    try {
      eval("(define 1)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("define: bad syntax in form: (define 1)", e.getMessage());
    }
    try {
      eval("(define a)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("define: bad syntax (missing expression after identifier) in form: (define a)", e.getMessage());
    }
    try {
      eval("(define a b c)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("define: bad syntax (multiple expressions after identifier) in form: (define a b c)", e.getMessage());
    }

    // internal define
    assertEquals(45L, eval("(let ((x 5))(define foo (lambda (y) (bar x y)))(define bar (lambda (a b) (+ (* a b) a)))(foo (+ x 3)))", env));
    try {
      eval("(foo 5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Unbound variable: foo", e.getMessage());
    }

    String d1 = "(define (test-internal-define)" +
        "  (let ((a 5) (b 7))" +
        "  (define (get-a) a)" +
        "  (define (get-b) b)" +
        "  (define (get-c) (+ (get-a) b))" +
        "  (+ (get-b) (get-c))))";
    eval(d1, env);
    assertEquals(19L, eval("(test-internal-define)", env));

    String d2 = "(define (test-internal-define2)" +
        "  (define (test2)" +
        "    (define (test4) 7)" +
        "    (define (test3) (test4))" +
        "    (+ 1 (test3)))" +
        "  (+ 1 (test2)))";
    eval(d2, env);
    assertEquals(9L, eval("(test-internal-define2)", env));

    // check that define is a top form in a body
    // TODO
    String d3 = "(let ((a 1)) a (define a 5) a)";
//    try {
//      eval(d3, env);
//      fail();
//    } catch (IllegalArgumentException e) {
//
//    }
  }

  @Test
  public void testEvalLambda() {
    String f1 = "(lambda ())";
    try {
      eval(f1, env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: bad lambda in form: " + f1, e.getMessage());
    }
  }

  @Test
  public void testEvalIf() {
    assertEquals(5L, eval("(if #t 5 0)",  env));
    assertEquals(5L, eval("(if #f 0 5)",  env));
    assertEquals(0L, eval("(if '() 0 5)", env));
    assertEquals(0L, eval("(if (not #f) 0 5)", env));
    assertEquals(5L, eval("(if (not (not (or #f #f))) 0 (+ 3 2))", env));
    assertEquals(new SCMSymbol("yes"), eval("(if (> 3 2) 'yes 'no)", env));
    assertEquals(new SCMSymbol("no"), eval("(if (> 2 3) 'yes 'no)", env));
    assertEquals(1L, eval("(if (> 3 2)(- 3 2)(+ 3 2))", env));
    assertEquals(UNSPECIFIED, eval("(if #f 5)", env));
    try {
      eval("(if)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("if: bad syntax (has 0 parts after keyword) in form: (if)", e.getMessage());
    }
    try {
      eval("(if 1)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("if: bad syntax (has 1 parts after keyword) in form: (if 1)", e.getMessage());
    }
    try {
      eval("(if 1 2 3 4 5)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("if: bad syntax (has 5 parts after keyword) in form: (if 1 2 3 4 5)", e.getMessage());
    }
  }

  @Test
  public void testEvalQuote() {
    assertEquals(0L, eval("'0", env));
    assertEquals(new SCMString("test"), eval("'\"test\"", env));
    assertEquals(SCMCons.<Object>list(new SCMSymbol(Quote.QUOTE.toString()), new SCMString("test")), eval("''\"test\"", env));
    assertEquals(list(new SCMSymbol("+"), 1L, 2L), eval("'(+ 1 2)", env));
    assertEquals(new SCMSymbol("0eab"), eval("'0eab", env));
    assertEquals(new SCMSymbol("000eab"), eval("'000eab", env));
  }

  @Test
  public void testEvalDottedPair() {
    assertEquals(2L, eval("(car (cdr '(1 2 3 . (2 3 4))))", env));
    assertEquals(cons(1L, 2L), eval("'(1 . 2)", env));
    assertEquals(cons(1L, cons(2L, cons(3L, 4L))), eval("'(1 2 3 . 4)", env));

    try {
      eval("'(1 2 3 . 4 5)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Error: bad dotted pair form: (1 2 3 . 4 5)", e.getMessage());
    }
    try {
      eval("'( . 1 2 3 4 5)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Error: bad dotted pair form: (. 1 2 3 4 5)", e.getMessage());
    }
  }

  @Test
  public void testEvalSet() {
    assertEquals(9L, eval("(let ((a 0)) (set! a 9) a)", env));
    assertEquals(19L, eval("(begin (define a 0) (set! a 9) (+ a 10))", env));
    try {
      eval("(begin (set! b 99) b)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Unbound variable: b", e.getMessage());
    }
  }

  @Test
  public void testEvalDo() {
    String doTest1 = "(do ((vec (make-vector 5))" +
        "     (i 0 (+ i 1)))" +
        "    ((= i 5) vec)" +
        "  (vector-set! vec i i))";
    assertEquals(new SCMVector(0L, 1L, 2L, 3L, 4L), eval(doTest1, env));

    String doTest2 = "(let ((x '(1 3 5 7 9)))" +
        "  (do ((x x (cdr x))" +
        "       (sum 0 (+ sum (car x))))" +
        "      ((null? x) sum)))";
    assertEquals(25L, eval(doTest2, env));

    String doTest3 = "(do ((a 5)) ((= a 0) \"DONE\") (set! a (- a 1)))";
    assertEquals(new SCMString("DONE"), eval(doTest3, env));

    try {
      eval("(do ((a 1) (b 2) (a 3)) (= 1 1) 5)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("let: duplicate identifier: a", e.getMessage());
    }
  }

  @Test
  public void testEvalLet() {
    assertEquals(124L, eval("(let ((c 123)) (+ c 1))", env));
    assertEquals(555L, eval("(let ((c 123) (b 432)) (+ c b))", env));
    try {
      eval("(let ((c 123) (c (+ 400 30 2))) (+ c b))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().contains("let: duplicate identifier: c"));
    }
    try {
      eval("(let ((c 123))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().contains("let: bad let in form:"));
    }
    try {
      eval("(let ((z 1) (b (+ z 1))) b)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Unbound variable: z"));
    }
  }

  @Test
  public void testEvalNamedLet() {
    assertEquals(120L, eval("(let fact ((n 5) (acc 1)) (if (= n 0) acc (fact (- n 1) (* acc n))))", env));
    assertEquals(12L,  eval("(let t ((x 5) (y 7)) (+ x y))", env));
    try {
      eval("(let fact ((n 5) (n 1)) (if (= n 0) acc (fact (- n 1) (* n n))))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().equals("let: duplicate identifier: n"));
    }
  }

  @Test
  public void testEvalLetStar() {
    assertEquals(2L, eval("(let* ((z 1) (b (+ z 1))) b)", env));
    try {
      eval("(let* ((c 123)))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue("Test bad let* form", e.getMessage().contains("let*: bad let* in form:"));
    }
  }

  @Test
  public void testEvalLetRec() {
    String letrec1 = "(letrec ((is-even? (lambda (n) (or (= n 0) (is-odd? (- n 1))))) " +
                     "         (is-odd?  (lambda (n) (and (not (= n 0)) (is-even? (- n 1))))))" +
                     "  (is-odd? 11))";
    assertEquals(TRUE, eval(letrec1, env));

    String letrec2 = "(letrec ((a a)) a)";
    assertEquals(UNSPECIFIED, eval(letrec2, env));

    String letrec3 = "(letrec ((a a)) (set! a 1) a)";
    assertEquals(1L, eval(letrec3, env));

    String letrec4 = "(eq? (letrec ((a a)) a) (if #f 0) (letrec ((a a)) a))";
    assertEquals(TRUE, eval(letrec4, env));
  }

  @Test
  public void testEvalCond() {
    assertEquals(UNSPECIFIED, eval("(cond)", env));
    // "Invalid clause in subform "
    try {
      eval("(cond 1)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("cond: bad syntax (invalid clause in subform) in form: (cond 1)", e.getMessage());
    }
    // "cond: else must be the last clause in subform"
    try {
      eval("(cond (else 1) (#t 5))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("cond: bad syntax (else must be the last clause in subform) in form: (cond (else 1) (#t 5))",
                   e.getMessage());
    }

    assertEquals(1L, eval("(cond (#f 5) ((not #t) 7) (else 1))", env));
    assertEquals(7L, eval("(cond (#f 5) ((not #f) 7) (else 1))", env));

    assertEquals(new SCMSymbol("greater"), eval("(cond ((> 3 2) 'greater)((< 3 2) 'less))", env));
    assertEquals(new SCMSymbol("equal"), eval("(cond ((> 3 3) 'greater)((< 3 3) 'less)(else 'equal))", env));
  }

  @Test
  public void testEvalCase() {
    try {
      eval("(case)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("case: bad syntax (source expression failed to match any pattern) in form: (case)", e.getMessage());
    }
    try {
      eval("(case 1 1)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("case: bad syntax (invalid clause in subform) in form: (case 1 1)", e.getMessage());

    }
    try {
      eval("(case (* 2 3) (else 'prime) ((1 4 6 8 9) 'composite))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("case: bad syntax (else must be the last clause in subform) in form: (case (* 2 3) (else (quote prime)) ((1 4 6 8 9) (quote composite)))", e.getMessage());
    }
    String caseform = "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))";
    assertEquals(new SCMSymbol("composite"), eval(caseform, env));

    caseform = "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 8 9) 'composite))";
    assertEquals(UNSPECIFIED, eval(caseform, env));

    caseform = "(case (* 2 3) ((2 3 5 7) 'prime) (else 'composite))";
    assertEquals(new SCMSymbol("composite"), eval(caseform, env));
  }

  @Test
  public void testEvalAnd() {
    assertEquals(TRUE, eval("(and)", env));
    assertEquals(1L, eval("(and 1)", env));
    assertEquals(TRUE, eval("(and (= 2 2) (> 2 1))", env));
    assertEquals(FALSE, eval("(and (= 2 2) (< 2 1))", env));
    assertEquals(SCMCons.<Object>list(new SCMSymbol("f"), new SCMSymbol("g")),
        eval("(and 1 2 'c '(f g)) ", env));
  }

  @Test
  public void testEvalOr() {
    assertEquals(FALSE, eval("(or)", env));
    assertEquals(TRUE, eval("(or (= 2 2) (> 2 1)) ", env));
    assertEquals(TRUE, eval("(or (= 2 2) (< 2 1))", env));
    assertEquals(FALSE, eval("(or #f #f #f)", env));
    assertEquals(SCMCons.<Object>list(new SCMSymbol("f"), new SCMSymbol("g")),
        eval("(or '(f g) 1 2)", env));
  }

  @Test
  public void testEvalBegin() {
    try {
      eval("(begin (set! x 5) (+ x 1))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Unbound variable: x", e.getMessage());
    }
    SCMOutputPort old = Main.getCurrentOutputPort();
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    Main.setCurrentOutputPort(new SCMOutputPort(new PrintStream(baos)));
    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display());
    assertEquals(UNSPECIFIED, eval("(begin (display \"4 plus 1 equals \")(display (+ 4 1)))", tempEnv));
    Main.setCurrentOutputPort(old);
  }

  @Test
  public void testEvalClassOf() {
    // class-of
    assertEquals(SCMClass.INTEGER, eval("(class-of 1)", env));
    assertEquals(SCMClass.INTEGER, eval("(class-of 9999999999999999999999999999999999)", env));
    assertEquals(SCMClass.DOUBLE,  eval("(class-of 9999999999999999999999999999999999.000)", env));
    assertEquals(SCMClass.DOUBLE,  eval("(class-of -1.0)", env));
    assertEquals(SCMClass.STRING,  eval("(class-of \"test\")", env));
    assertEquals(SCMClass.CHARACTER, eval("(class-of #\\A)", env));
    assertEquals(SCMClass.SYMBOL, eval("(class-of 'test)", env));
    assertEquals(SCMClass.CLASS, eval("(class-of (class-of 'test))", env));
    assertEquals(SCMClass.VECTOR, eval("(class-of #(1 2 3))", env));
    assertEquals(SCMClass.PAIR, eval("(class-of '(1 2 3))", env));
    assertEquals(SCMClass.NIL, eval("(class-of '())", env));
    assertEquals(SCMClass.BOOLEAN, eval("(class-of #t)", env));
    assertEquals(SCMClass.BOOLEAN, eval("(class-of (= 1 2))", env));
    assertEquals(SCMClass.PROCEDURE, eval("(class-of +)", env));
    assertEquals(SCMClass.PROCEDURE, eval("(class-of (lambda (n) n))", env));
    assertEquals(SCMClass.PROMISE, eval("(class-of (delay (+ 1 2)))", env));
  }

  @Test
  public void testEvalError() {
    // error
    try {
      eval("(error \"boom\")", env).getClass();
      fail();
    } catch (SCMError e) {
      assertEquals("boom", e.getMessage());
    }
  }

  @Test
  public void testRedefineSpecialForms() {
    IEnvironment tempEnv = new DefaultEnvironment();
    eval("(define (and . args) #f)", tempEnv);
    eval("(define begin 5)", tempEnv);
    eval("(define if 4)", tempEnv);
    eval("(define quote 3)", tempEnv);
    eval("(define let 2)", tempEnv);
    eval("(define lambda 1)", tempEnv);
    assertEquals(15L, eval("(+ begin if quote let lambda)", tempEnv));
    assertEquals(3L, eval("(and 1 2 3)", env));
    assertEquals(FALSE, eval("(and 1 2 3 4)", tempEnv));
  }

  @Test
  public void testQuasiquote() {
    assertEquals(1L, eval("(quasiquote 1)", env));
    assertEquals(1L, eval("`1", env));
    assertEquals(15.5, eval("(quasiquote 15.5)", env));
    assertEquals(15.5, eval("`15.5", env));
    assertEquals(new SCMString("test"), eval("(quasiquote \"test\")", env));
    assertEquals(new SCMString("test"), eval("`\"test\"", env));
    assertEquals(new SCMSymbol("quote"), eval("(quasiquote quote)", env));
    assertEquals(list(new SCMSymbol("+"), 1L, 2L), eval("`(+ 1 2)", env));
    assertEquals(3L, eval("`,(+ 1 2)", env));
    assertEquals(13L, eval("`,(+ 1 (* 3 4))", env));
    assertEquals(13L, eval("(quasiquote ,(+ 1 (* 3 4)))", env));
    assertEquals(13L, eval("(quasiquote (unquote (+ 1 (* 3 4))))", env));
    assertEquals(list(1L, 3L, 4L), eval("`(1 ,(+ 1 2) 4)", env));
    assertEquals(list(1L, list(new SCMSymbol("quasiquote"), list(new SCMSymbol("unquote"), list(new SCMSymbol("+"), 1L, 5L))), 4L),
                 eval("`(1 `,(+ 1 ,(+ 2 3)) 4)", env));

    assertEquals(list(1L, list(new SCMSymbol("quasiquote"), list(new SCMSymbol("unquote"), list(new SCMSymbol("+"), 1L, new SCMVector(new SCMSymbol("+"), 2L, 3L)))), 4L),
                 eval("`(1 `,(+ 1 ,#(+ 2 3)) 4)", env));

    assertEquals(list(new SCMSymbol("list"), 3L, 4L), eval("`(list ,(+ 1 2) 4)", env));
    assertEquals(list(new SCMSymbol("list"), new SCMSymbol("a"), list(new SCMSymbol("quote"), new SCMSymbol("a"))),
                 eval("(let ((name 'a)) `(list ,name ',name))", env));

    assertEquals(list(new SCMSymbol("a"), 3L, 4L, 5L, 6L, new SCMSymbol("b")),
                 eval("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", env));

    assertEquals(cons(list(new SCMSymbol("foo"), 7L), new SCMSymbol("cons")), eval("`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))", env));
    assertEquals(5L, eval("`,(+ 2 3)", env));

    assertEquals(list(1L, 2L, 3L), eval("`(1 ,@(list 2 3))", env));
    assertEquals(list(1L, 2L, 7L), eval("`(1 2 ,`,(+ 3 4))", env));
    assertEquals(1L, eval("`,`,`,`,`,1", env));
    assertEquals(1L, eval("`,`,`,`,`,`1", env));
    assertEquals(3L, eval("`,`,`,`,`,(+ 1 2)", env));
    assertEquals(list(new SCMSymbol("+"), 1L, 2L), eval("`,`,`,`,`,`(+ 1 2)", env));

    assertEquals(new SCMVector(1L, 5L), eval("`#(1 ,(+ 2 3))", env));
    assertEquals(new SCMVector(1L, list(new SCMSymbol("quasiquote"), list(new SCMSymbol("unquote"), list(1L, 5L)))),
                 eval("`#(1 `,(1 ,(+ 2 3)))", env));

    assertEquals(eval("'foo", env), eval("`(,@'() . foo)", env));
    assertEquals(cons(new SCMSymbol("unquote-splicing"), new SCMSymbol("foo")), eval("`(unquote-splicing . foo)", env));
    assertEquals(cons(new SCMSymbol("unquote"), cons(1L, 2L)), eval("`(unquote 1 . 2)", env));

    assertEquals(NIL, eval("`()", env));
    assertEquals(new SCMVector(), eval("`#()", env));
    assertEquals(list(1L, 2L, list(NIL)), eval("`(1 2 ())", env));
    assertEquals(list(1L, 2L, list(new SCMSymbol("quote"), NIL)), eval("`(1 2 '())", env));

    try {
      eval("unquote", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("Unexpected syntax in form: unquote", e.getMessage());
    }
    try {
      eval("(quasiquote (unquote 1 2))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("unquote: expects exactly one expression", e.getMessage());
    }
    try {
      eval("`#(1 unquote 2)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: List, actual: (1 . 2)", e.getMessage());
    }
  }

  @Test
  public void testTime() {
    /*
     * 1) Procedure inlining (zero? pair? null? etc.)
     *
     * 2) Reduce & Optimize (cache) expensive calls:
     *
     * -------------------+----------
     * Method             | Time (ms)
     * -------------------+----------
     * Environment.find() |  71,818
     * Evaluator.evlis()  |  37,296
     * Reader.nextToken() |  29,801
     * Environment.put()  |  11,328
     * -------------------+----------
     *
     * Env.find:
     * Before:
     *
     *  (perf 1)
     *    [ENV.FIND: perf]     [ENV.FIND: =]        [ENV.FIND: perf]   [ENV.FIND: perf]
     *    [ENV.FIND: perf]     [ENV.FIND: MISS!]    [ENV.FIND: zero?]  [ENV.FIND: perf]
     *    [ENV.FIND: if]       [ENV.FIND: =]        [ENV.FIND: MISS!]  [ENV.FIND: zero?]
     *    [MISS!]              [ENV.FIND: =]        [ENV.FIND: zero?]  [ENV.FIND: MISS!]
     *    [ENV.FIND: if]       [ENV.FIND: MISS!]    [ENV.FIND: zero?]  [ENV.FIND: zero?]
     *    [ENV.FIND: zero?]    [ENV.FIND: =]        [ENV.FIND: MISS!]  [ENV.FIND: zero?]
     *    [MISS!]              [ENV.FIND: n]        [ENV.FIND: zero?]  [ENV.FIND: MISS!]
     *    [ENV.FIND: zero?]    [ENV.FIND: perf]     [ENV.FIND: n]      [ENV.FIND: zero?]
     *    [ENV.FIND: zero?]    [ENV.FIND: MISS!]    [ENV.FIND: n]      [ENV.FIND: n]
     *    [MISS!]              [ENV.FIND: perf]     [ENV.FIND: perf]   [ENV.FIND: n]
     *    [ENV.FIND: zero?]    [ENV.FIND: perf]     [ENV.FIND: MISS!]  [ENV.FIND: n]
     *    [ENV.FIND: n]        [ENV.FIND: MISS!]    [ENV.FIND: perf]   [ENV.FIND: zero?]
     *    [ENV.FIND: =]        [ENV.FIND: perf]     [ENV.FIND: perf]   [ENV.FIND: MISS!]
     *    [MISS!]              [ENV.FIND: -]        [ENV.FIND: MISS!]  [ENV.FIND: zero?]
     *    [ENV.FIND: =]        [ENV.FIND: MISS!]    [ENV.FIND: perf]   [ENV.FIND: zero?]
     *    [ENV.FIND: =]        [ENV.FIND: -]        [ENV.FIND: n]      [ENV.FIND: MISS!]
     *    [MISS!]              [ENV.FIND: -]        [ENV.FIND: zero?]  [ENV.FIND: zero?]
     *    [ENV.FIND: =]        [ENV.FIND: MISS!]    [ENV.FIND: MISS!]  [ENV.FIND: n]
     *    [ENV.FIND: n]        [ENV.FIND: -]        [ENV.FIND: zero?]  [ENV.FIND: n]
     *    [ENV.FIND: perf]     [ENV.FIND: n]        [ENV.FIND: zero?]
     *    [MISS!]              [ENV.FIND: zero?]    [ENV.FIND: MISS!]
     *    [ENV.FIND: perf]     [ENV.FIND: MISS!]    [ENV.FIND: zero?]
     *    [ENV.FIND: perf]     [ENV.FIND: zero?]    [ENV.FIND: n]
     *    [MISS!]              [ENV.FIND: zero?]    [ENV.FIND: n]
     *    [ENV.FIND: perf]     [ENV.FIND: MISS!]
     *    [ENV.FIND: -]        [ENV.FIND: zero?]
     *    [MISS!]              [ENV.FIND: n]
     *    [ENV.FIND: -]        [ENV.FIND: =]
     *    [ENV.FIND: -]        [ENV.FIND: MISS!]
     *    [MISS!]              [ENV.FIND: =]
     *    [ENV.FIND: -]        [ENV.FIND: =]
     *    [ENV.FIND: n]        [ENV.FIND: MISS!]
     *    [ENV.FIND: if]       [ENV.FIND: =]
     *    [MISS!]              [ENV.FIND: n]
     *    [ENV.FIND: if]
     *    [ENV.FIND: zero?]
     *    [MISS!]
     *    [ENV.FIND: zero?]
     *    [ENV.FIND: zero?]
     *    [MISS!]
     *    [ENV.FIND: zero?]
     *    [ENV.FIND: n]
     *    [ENV.FIND: =]
     *    [MISS!]
     *    [ENV.FIND: =]
     *    [ENV.FIND: =]
     *    [MISS!]
     *    [ENV.FIND: =]
     *    [ENV.FIND: n]
     *    $1 = "DONE"
     */
    String form = "(time" +
                  " (define (perf n)" +
                  "   (if (zero? n)" +
                  "       \"DONE\"" +
                  "     (perf (- n 1))))" +
                  " (perf 10000))";
    assertEquals(UNSPECIFIED, eval(form, env));
  }

  @Test
  public void testDuplicateArgumentsAreNotAllowed() {
    try {
      eval("(lambda (a a) a)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: duplicate argument identifier `a`", e.getMessage());
    }
    try {
      eval("(define (a b b) b)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: duplicate argument identifier `b`", e.getMessage());
    }
  }
}
