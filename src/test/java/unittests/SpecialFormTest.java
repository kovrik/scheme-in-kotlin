package unittests;

import core.Repl;
import core.environment.DefaultEnvironment;
import core.environment.Environment;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ReentrantDelayException;
import core.exceptions.ThrowableWrapper;
import core.exceptions.UndefinedIdentifierException;
import core.procedures.io.Display;
import core.procedures.math.Addition;
import core.scm.BigRatio;
import core.scm.Cons;
import core.scm.Delay;
import core.scm.Error;
import core.scm.MutableString;
import core.scm.MutableVector;
import core.scm.OutputPort;
import core.scm.Procedure;
import core.scm.Symbol;
import core.scm.Void;
import core.scm.specialforms.Quote;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.math.BigInteger;

import static core.scm.Cons.EMPTY;
import static core.scm.Cons.cons;
import static core.scm.Cons.list;
import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class SpecialFormTest extends AbstractTest {

  @Test
  public void testEvalImplicitBegin() {
    assertEquals(3L, eval("((lambda () 1 2 (+ 1 2)))", env));
    assertEquals(3L, eval("(let    () 1 2 (+ 1 2))", env));
    assertEquals(3L, eval("(let*   () 1 2 (+ 1 2))", env));
    assertEquals(3L, eval("(letrec () 1 2 (+ 1 2))", env));
    eval("(define (a) 1 2 (+ 1 2))", env);
    assertEquals(3L, eval("(a)", env));
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
    assertEquals("test", eval("(force (delay \"test\"))", env));
    assertEquals(10L, eval("(force (delay (+ 5 2 (* 1 3))))", env));
    assertEquals(Delay.class, eval("(delay 1.0)", env).getClass());
    assertEquals(TRUE,  eval("(promise? (delay 1.0))", env));
    assertEquals(FALSE, eval("(promise? (future 1.0))", env));
    assertEquals(FALSE, eval("(future?  (delay 1.0))", env));
    assertEquals(TRUE,  eval("(future?  (future 1.0))", env));
    assertEquals(3L, eval("(force (delay (+ 1 2)))", env));
    assertEquals(list(3L, 3L), eval("(let ((p (delay (+ 1 2))))(list (force p) (force p)))", env));

    eval("(define perr (delay (error \"BOOM\")))", env);
    try {
      eval("(force perr)", env);
      fail();
    } catch (Error e) {
      assertEquals("BOOM", e.getMessage());
    }
    try {
      eval("(force perr)", env);
      fail();
    } catch (Error e) {
      assertEquals("BOOM", e.getMessage());
    }
    try {
      eval("(delay)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("delay: bad syntax in form: (delay)", e.getMessage());
    }
    /* Check that re-entrant promises are not allowed
     * See http://lambda-the-ultimate.org/node/4686A
     */
    eval("(define x 0)", env);
    String conundrum = "(define p" +
                       "  (delay" +
                       "    (if (= x 5)" +
                       "      x" +
                       "      (begin" +
                       "        (set! x (+ x 1))" +
                       "        (force p)" +
                       "        (set! x (+ x 1))" +
                       "        x))))";
    eval(conundrum, env);
    try {
      eval("(force p)", env);
      fail();
    } catch (ReentrantDelayException e) {
      // success
    }
  }

  @Test
  public void testEvalProcedure() {
    assertEquals(Procedure.class, eval("(lambda () #t)", env).getClass());
    assertEquals(TRUE, eval("((lambda () #t))", env));
    assertEquals(6L, eval("((lambda (n) (+ n 1)) 5)", env));

    eval("(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))", env);
    assertEquals(8L, eval("(fib 5)", env));

    assertEquals(6L, eval("((lambda (n) (+ n 1)) 5)", env));

    // rest arguments
    assertEquals(list(1L, 2L, 3L), eval("((lambda x x) 1 2 3)", env));
    assertEquals(EMPTY, eval("((lambda x x))", env));
    assertEquals(1L, eval("((lambda x (car x)) 1 2 3)", env));
    assertEquals(1L, eval("((lambda (f s . rs) f) 1 2 3 4)", env));
    assertEquals(2L, eval("((lambda (f s . rs) s) 1 2 3 4)", env));
    assertEquals(list(3L, 4L), eval("((lambda (f s . rs) rs) 1 2 3 4)", env));
  }

  @Test
  public void testEvalDefine() {
    eval("(define a 5)", env);
    assertEquals(5L, eval("a", env));
    assertEquals(Symbol.intern("b"), eval("(define b 7)", env));
    assertEquals(Symbol.intern("c"), eval("(define (c n) (+ n 7))", env));

    eval("(define edl (lambda (n) (+ n 1)))", env);
    assertEquals(2L, eval("(edl 1)", env));

    // variadic
    eval("(define edlv (lambda args args))", env);
    assertEquals(Cons.list(1L, 2L, 3L, 4L, 5L), eval("(edlv 1 2 3 4 5)", env));

    // variadic define
    eval("(define (edv1 first second . rest) rest)", env);
    assertEquals(Cons.list(2L, 3L, 4L, 5L), eval("(edv1 0 1 2 3 4 5)", env));

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
      assertEquals("define: bad syntax in form: (define a)", e.getMessage());
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
    } catch (UndefinedIdentifierException e) {
      // expected
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

    assertEquals(list(3L, 4L, 5L), eval("((lambda (a b c . d) d) 0 1 2 3 4 5)", env));

    // TODO Check Definition context
  }

  @Test
  public void testEvalLambda() {
    String f1 = "(lambda ())";
    try {
      eval(f1, env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: bad syntax in form: " + f1, e.getMessage());
    }
    String f2 = "(lambda 1 2 3 4)";
    try {
      eval(f2, env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: bad argument sequence (1) in form: " + f2, e.getMessage());
    }
  }

  @Test
  public void testEvalIf() {
    assertEquals(5L, eval("(if #t 5 0)",  env));
    assertEquals(5L, eval("(if #f 0 5)",  env));
    assertEquals(0L, eval("(if '() 0 5)", env));
    assertEquals(0L, eval("(if (not #f) 0 5)", env));
    assertEquals(5L, eval("(if (not (not (or #f #f))) 0 (+ 3 2))", env));
    assertEquals(Symbol.intern("yes"), eval("(if (> 3 2) 'yes 'no)", env));
    assertEquals(Symbol.intern("no"), eval("(if (> 2 3) 'yes 'no)", env));
    assertEquals(1L, eval("(if (> 3 2)(- 3 2)(+ 3 2))", env));
    assertEquals(Void.VOID, eval("(if #f 5)", env));
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
    assertEquals("test", eval("'\"test\"", env));
    assertEquals(Cons.list(Symbol.intern(Quote.QUOTE.toString()), "test"), eval("''\"test\"", env));
    assertEquals(list(Symbol.intern("+"), 1L, 2L), eval("'(+ 1 2)", env));
    assertEquals(Symbol.intern("0eab"), eval("'0eab", env));
    assertEquals(Symbol.intern("000eab"), eval("'000eab", env));
  }

  @Test
  public void testEvalDottedPair() {
    assertEquals(2L, eval("(car (cdr '(1 2 3 . (2 3 4))))", env));
    assertEquals(cons(1L, 2L), eval("'(1 . 2)", env));
    assertEquals(cons(1L, cons(2L, cons(3L, 4L))), eval("'(1 2 3 . 4)", env));
    assertEquals(6L, eval("(+ . (1 2 3))", env));
    assertEquals(6L, eval("(+ . (1 . (2 3)))", env));
    assertEquals(6L, eval("(+ . (1 . (2 . (3))))", env));
    assertEquals(6L, eval("(+ . (1 . (2 . (3 . ()))))", env));
    try {
      eval("'(1 2 3 . 4 5)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: illegal use of '.'", e.getMessage());
    }
//    try {
//      eval("'( . 1 2 3 4 5)", env);
//      fail();
//    } catch (IllegalSyntaxException e) {
//      assertEquals("read: illegal use of '.'", e.getMessage());
//    }
  }

  @Test
  public void testEvalSet() {
    assertEquals(9L, eval("(let ((a 0)) (set! a 9) a)", env));
    assertEquals(19L, eval("(begin (define a 0) (set! a 9) (+ a 10))", env));
    try {
      eval("(begin (set! b 99) b)", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
    }
  }

  @Test
  public void testEvalDo() {
    String doTest1 = "(do ((vec (make-vector 5))" +
        "     (i 0 (+ i 1)))" +
        "    ((= i 5) vec)" +
        "  (vector-set! vec i i))";
    assertEquals(new MutableVector(0L, 1L, 2L, 3L, 4L), eval(doTest1, env));

    String doTest2 = "(let ((x '(1 3 5 7 9)))" +
        "  (do ((x x (cdr x))" +
        "       (sum 0 (+ sum (car x))))" +
        "      ((empty? x) sum)))";
    assertEquals(25L, eval(doTest2, env));

    String doTest3 = "(do ((a 5)) ((= a 0) \"DONE\") (set! a (- a 1)))";
    assertEquals("DONE", eval(doTest3, env));

    assertEquals(Void.VOID, eval("(do ((i 1 (add1 i))) ((> i 4)) (void i))", env));
    assertEquals("DONE", eval("(do ((i 1 (add1 i))) ((> i 4) \"DONE\") (void i))", env));

    try {
      eval("(do ((a 1) (b 2) (a 3)) (= 1 1) 5)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("let: bad syntax (duplicate identifier: a) in form: (do ((a 1) (b 2) (a 3)) (= 1 1) 5)", e.getMessage());
    }
    /* Check that each iteration establishes bindings to fresh locations
     * See https://www.gnu.org/software/guile/manual/html_node/while-do.html */
    eval("(define lst '())", env);
    eval("(do ((i 1 (+ i 1)))" +
         "    ((> i 4))" +
         "  (set! lst (cons (lambda () i) lst)))", env);
    assertEquals(list(4L, 3L, 2L, 1L), eval("(map (lambda (proc) (proc)) lst)", env));
  }

  @Test
  public void testEvalLet() {
    assertEquals(124L, eval("(let ((c 123)) (+ c 1))", env));
    assertEquals(555L, eval("(let ((c 123) (b 432)) (+ c b))", env));
    try {
      eval("(let ((a 1) (b a) (c b)) c)", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
    }
    try {
      eval("(let ((c 123) (c (+ 400 30 2))) (+ c b))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("let: bad syntax (duplicate identifier: c) in form: (let ((c 123) (c (+ 400 30 2))) (+ c b))",
                   e.getMessage());
    }
    try {
      eval("(let ((c 123))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("let: bad syntax in form: (let ((c 123)))", e.getMessage());
    }
    try {
      eval("(let ((z 1) (b (+ z 1))) b)", env);
      fail();
    } catch (RuntimeException e) {
      // expected
    }
    try {
      eval("(let ((a a)) a)", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
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
      assertEquals("let: bad syntax (duplicate identifier: n) in form: (let fact ((n 5) (n 1)) (if (= n 0) acc (fact (- n 1) (* n n))))", e.getMessage());
    }
  }

  @Test
  public void testEvalLetSeq() {
    assertEquals(2L, eval("(let* ((z 1) (b (+ z 1))) b)", env));
    assertEquals(1L, eval("(let* ((a 1) (b a) (c b)) c)", env));
    try {
      eval("(let* ((c 123)))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("let*: bad syntax in form: (let* ((c 123)))", e.getMessage());
    }
    try {
      eval("(let* ((a a)) a)", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
    }
  }

  @Test
  public void testEvalLetRec() {
    String letrec1 = "(letrec ((is-even? (lambda (n) (or (= n 0) (is-odd? (- n 1))))) " +
                     "         (is-odd?  (lambda (n) (and (not (= n 0)) (is-even? (- n 1))))))" +
                     "  (is-odd? 11))";
    assertEquals(TRUE, eval(letrec1, env));
    assertEquals(1L, eval("(letrec ((a 1) (b a) (c b)) c)", env));
    try {
      eval("(letrec ((a a)) a)", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
    }

    try {
      eval("(letrec ((a a)) (set! a 1) a)", env);
      fail();
    } catch (RuntimeException e) {
      // expected
    }

    try {
      eval("(eq? (letrec ((a a)) a) (if #f 0) (letrec ((a a)) a))", env);
      fail();
    } catch (RuntimeException e) {
      // expected
    }
  }

  @Test
  public void testEvalCond() {
    assertEquals(Void.VOID, eval("(cond)", env));
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

    assertEquals(Symbol.intern("greater"), eval("(cond ((> 3 2) 'greater)((< 3 2) 'less))", env));
    assertEquals(Symbol.intern("equal"), eval("(cond ((> 3 3) 'greater)((< 3 3) 'less)(else 'equal))", env));
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
    assertEquals(Symbol.intern("composite"), eval(caseform, env));

    caseform = "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 8 9) 'composite))";
    assertEquals(Void.VOID, eval(caseform, env));

    caseform = "(case (* 2 3) ((2 3 5 7) 'prime) (else 'composite))";
    assertEquals(Symbol.intern("composite"), eval(caseform, env));
  }

  @Test
  public void testEvalAnd() {
    assertEquals(TRUE, eval("(and)", env));
    assertEquals(1L, eval("(and 1)", env));
    assertEquals(TRUE, eval("(and (= 2 2) (> 2 1))", env));
    assertEquals(FALSE, eval("(and (= 2 2) (< 2 1))", env));
    assertEquals(Cons.<Object>list(Symbol.intern("f"), Symbol.intern("g")),
                 eval("(and 1 2 'c '(f g)) ", env));
  }

  @Test
  public void testEvalOr() {
    assertEquals(FALSE, eval("(or)", env));
    assertEquals(TRUE, eval("(or (= 2 2) (> 2 1)) ", env));
    assertEquals(TRUE, eval("(or (= 2 2) (< 2 1))", env));
    assertEquals(FALSE, eval("(or #f #f #f)", env));
    assertEquals(Cons.<Object>list(Symbol.intern("f"), Symbol.intern("g")),
                 eval("(or '(f g) 1 2)", env));
  }

  @Test
  public void testEvalBegin() {
    assertEquals(Void.VOID, eval("(begin)", env));
    assertEquals(Void.VOID, eval("(begin (begin))", env));
    assertEquals(1L, eval("(begin 1)", env));
    assertEquals(3L, eval("(begin 1 2 3)", env));
    try {
      eval("(begin (set! x 5) (+ x 1))", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
    }
    OutputPort old = Repl.getCurrentOutputPort();
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    Repl.setCurrentOutputPort(new OutputPort(new PrintStream(baos)));
    DefaultEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(Symbol.intern("display"), new Display());
    assertEquals(Void.VOID, eval("(begin (display \"4 plus 1 equals \")(display (+ 4 1)))", tempEnv));
    Repl.setCurrentOutputPort(old);
  }

  @Test
  public void testEvalClassOf() {
    // class-of
    assertEquals(Long.class, eval("(class-of 1)", env));
    assertEquals(Long.class, eval("(class-of -2341)", env));
    assertEquals(BigInteger.class, eval("(class-of 9999999999999999999999999999999999)", env));
    assertEquals(Double.class, eval("(class-of -1.0)", env));
    assertEquals(Double.class, eval("(class-of -1.5)", env));
    assertEquals(BigDecimal.class, eval("(class-of 9999999999999999999999999999999999.000)", env));
    assertEquals(BigDecimal.class, eval("(class-of 9999999999999999999999999999999999.430)", env));
    assertEquals(Long.class, eval("(class-of 1/1)", env));
    assertEquals(BigRatio.class, eval("(class-of -2341/345)", env));
    assertEquals(String.class,  eval("(class-of \"test\")", env));
    assertEquals(MutableString.class, eval("(class-of (string #\\a))", env));
    assertEquals(Character.class, eval("(class-of #\\A)", env));
    assertEquals(Symbol.class, eval("(class-of 'test)", env));
    assertEquals(Class.class, eval("(class-of (class-of 'test))", env));
    assertEquals(MutableVector.class , eval("(class-of #(1 2 3))", env));
    assertEquals(Cons.class, eval("(class-of '(1 2 3))", env));
//    assertEquals(Cons.class, eval("(class-of '())", env));
    assertEquals(Boolean.class, eval("(class-of #t)", env));
    assertEquals(Boolean.class, eval("(class-of (= 1 2))", env));
    assertEquals(Addition.class, eval("(class-of +)", env));
    assertEquals(Procedure.class, eval("(class-of (lambda (n) n))", env));
    assertEquals(Delay.class, eval("(class-of (delay (+ 1 2)))", env));
  }

  @Test
  public void testEvalError() {
    // error
    try {
      eval("(error \"boom\")", env).getClass();
      fail();
    } catch (Error e) {
      assertEquals("boom", e.getMessage());
    }
  }

  @Test
  public void testRedefineSpecialForms() {
    Environment tempEnv = new DefaultEnvironment();
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
    assertEquals("test", eval("(quasiquote \"test\")", env));
    assertEquals("test", eval("`\"test\"", env));
    assertEquals(Symbol.intern("quote"), eval("(quasiquote quote)", env));
    assertEquals(list(Symbol.intern("+"), 1L, 2L), eval("`(+ 1 2)", env));
    assertEquals(3L, eval("`,(+ 1 2)", env));
    assertEquals(13L, eval("`,(+ 1 (* 3 4))", env));
    assertEquals(13L, eval("(quasiquote ,(+ 1 (* 3 4)))", env));
    assertEquals(13L, eval("(quasiquote (unquote (+ 1 (* 3 4))))", env));
    assertEquals(list(1L, 3L, 4L), eval("`(1 ,(+ 1 2) 4)", env));
    assertEquals(list(1L, list(Symbol.intern("quasiquote"), list(Symbol.intern("unquote"), list(Symbol.intern("+"), 1L, 5L))), 4L),
                 eval("`(1 `,(+ 1 ,(+ 2 3)) 4)", env));

    assertEquals(list(1L, list(Symbol.intern("quasiquote"), list(Symbol.intern("unquote"), list(Symbol.intern("+"), 1L, new MutableVector(
                   Symbol.intern("+"), 2L, 3L)))), 4L),
                 eval("`(1 `,(+ 1 ,'[+ 2 3]) 4)", env));

    assertEquals(list(Symbol.intern("list"), 3L, 4L), eval("`(list ,(+ 1 2) 4)", env));
    assertEquals(list(Symbol.intern("list"), Symbol.intern("a"), list(Symbol.intern("quote"), Symbol.intern("a"))),
                 eval("(let ((name 'a)) `(list ,name ',name))", env));

    assertEquals(list(Symbol.intern("a"), 3L, 4L, 5L, 6L, Symbol.intern("b")),
                 eval("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", env));

    assertEquals(cons(list(Symbol.intern("foo"), 7L), Symbol.intern("cons")), eval("`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))", env));
    assertEquals(5L, eval("`,(+ 2 3)", env));

    assertEquals(list(1L, 2L, 3L), eval("`(1 ,@(list 2 3))", env));
    assertEquals(list(1L, 2L, 7L), eval("`(1 2 ,`,(+ 3 4))", env));
    assertEquals(1L, eval("`,`,`,`,`,1", env));
    assertEquals(1L, eval("`,`,`,`,`,`1", env));
    assertEquals(3L, eval("`,`,`,`,`,(+ 1 2)", env));
    assertEquals(list(Symbol.intern("+"), 1L, 2L), eval("`,`,`,`,`,`(+ 1 2)", env));

    assertEquals(new MutableVector(1L, 5L), eval("`[1 ,(+ 2 3)]", env));
    assertEquals(new MutableVector(1L, list(Symbol.intern("quasiquote"), list(Symbol.intern("unquote"), list(1L, 5L)))),
                 eval("`[1 `,(1 ,(+ 2 3))]", env));

    assertEquals(eval("'foo", env), eval("`(,@'() . foo)", env));
    assertEquals(cons(Symbol.intern("unquote-splicing"), Symbol.intern("foo")), eval("`(unquote-splicing . foo)", env));
    assertEquals(cons(Symbol.intern("unquote"), cons(1L, 2L)), eval("`(unquote 1 . 2)", env));

    assertEquals(EMPTY, eval("`()", env));
    assertEquals(new MutableVector(), eval("`#()", env));
    assertEquals(list(1L, 2L, list(EMPTY)), eval("`(1 2 ())", env));
    assertEquals(list(1L, 2L, list(Symbol.intern("quote"), EMPTY)), eval("`(1 2 '())", env));

    try {
      eval("unquote", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("unquote: bad syntax in form: unquote", e.getMessage());
    }
    try {
      eval("(quasiquote (unquote 1 2))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("unquote: bad syntax (unquote expects exactly one expression) in form: (unquote 1 2)", e.getMessage());
    }
    try {
      eval("`[1 unquote 2]", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: illegal use of '.'", e.getMessage());
    }
  }

  @Test
  public void testTime() {
    OutputPort old = Repl.getCurrentOutputPort();
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    Repl.setCurrentOutputPort(new OutputPort(new PrintStream(baos)));
    String form = "(time" +
                  " (define (perf n)" +
                  "   (if (zero? n)" +
                  "       \"DONE\"" +
                  "     (perf (- n 1))))" +
                  " (perf 10000))";
    assertEquals("DONE", eval(form, env));
    Repl.setCurrentOutputPort(old);
  }

  @Test
  public void testDuplicateArgumentsAreNotAllowed() {
    try {
      eval("(lambda (a a) a)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: bad syntax (duplicate argument name: a) in form: (lambda (a a) a)", e.getMessage());
    }
    try {
      eval("(define (a b b) b)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("lambda: bad syntax (duplicate argument name: b) in form: (lambda (b b) b)", e.getMessage());
    }
  }

  @Test
  public void testThrow() {
    try {
      eval("(throw (new Exception))", env);
      fail();
    } catch (ThrowableWrapper e) {
      assertEquals(Exception.class, e.getCause().getClass());
    }
    try {
      eval("(throw (new NullPointerException \"BOOM\"))", env);
      fail();
    } catch (ThrowableWrapper e) {
      assertEquals(NullPointerException.class, e.getCause().getClass());
      assertEquals("BOOM", e.getCause().getMessage());
    }
    try {
      eval("(throw (new StackOverflowError))", env);
      fail();
    } catch (ThrowableWrapper e) {
      assertEquals(StackOverflowError.class, e.getCause().getClass());
    }
  }

  @Test
  public void testTryCatchFinally() {
    assertEquals(null, eval("(try)", env));
    assertEquals(null, eval("(try (catch Exception e))", env));
    assertEquals(null, eval("(try (finally))", env));
    assertEquals(null, eval("(try (catch Exception e) (finally))", env));
    assertEquals(6L, eval("(try 6)", env));
    assertEquals(6L, eval("(try (+ 1 2 3) (finally))", env));
    assertEquals(3L, eval("(try 1 2 3 (catch Exception e) (finally))", env));
    assertEquals(3L, eval("(let ((a 0)) (try (set! a (inc a)) (throw (new Exception)) (catch Exception e (set! a (inc a))) (finally (set! a (inc a)))) a)", env));
    assertEquals(2L, eval("(let ((a 0)) (try (set! a (inc a)) (throw (new Exception)) (catch Exception e (set! a (inc a)))) a)", env));
    assertEquals(2L, eval("(let ((a 0)) (try (set! a (inc a)) (finally (set! a (inc a)))) a)", env));
    assertEquals(6L, eval("(let ((a 0)) (try (set! a 5) (finally (set! a (inc a)))) a)", env));
    String[] illegalSyntax = {"(try 1 2 (finally) 3 (catch Exception e) (finally))",
      "(try 1 2 3 (catch Exception e) (finally) (catch Exception e))", "(try 1 (catch Exception e) 2 3)",
      "(try 1 (catch Exception))", "(try 1 (catch))", "(try 1 (catch 1))", "(try 1 (catch a))", "(try 1 (catch Exception 123))"
    };
    for (String illegal : illegalSyntax) {
      try {
        eval(illegal, env);
      } catch (IllegalSyntaxException e) {
        // success
      }
    }
  }

  @Test
  public void testNilValue() {
    assertEquals(null, eval("(let ((a nil)) a)", env));
    assertEquals(null, eval("(let* ((a nil)) a)", env));
    assertEquals(null, eval("(letrec ((a nil)) a)", env));
    assertEquals(null, eval("(begin (define nv nil) nv)", env));
    assertEquals(null, eval("(begin (define nv 5) (set! nv nil) nv)", env));
    try {
      eval("some-undefined-identifier", env);
    } catch (UndefinedIdentifierException e) {
      // expected
    }
    assertEquals(null, eval("(begin (define some-undefined-identifier nil) some-undefined-identifier)", env));
  }

  @Test
  public void testInstanceOf() {
    assertEquals(TRUE,  eval("(instance? String \"str\")", env));
    assertEquals(TRUE,  eval("(instance? Object \"str\")", env));
    assertEquals(TRUE,  eval("(instance? Object (new NullPointerException))", env));
    assertEquals(TRUE,  eval("(instance? Class (.getClass 1))", env));
    assertEquals(TRUE,  eval("(instance? (.getClass []) [])", env));
    assertEquals(TRUE,  eval("(instance? java.math.BigDecimal (new java.math.BigDecimal 10))", env));
    assertEquals(FALSE, eval("(instance? String 1)", env));
    assertEquals(FALSE, eval("(instance? String [])", env));
    assertEquals(FALSE, eval("(instance? String (new Object))", env));
    assertEquals(FALSE, eval("(instance? Number \"\")", env));
    assertEquals(FALSE, eval("(instance? (.getClass {}) #{})", env));
  }
}
