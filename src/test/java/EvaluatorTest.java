import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.parser.IParser;
import core.parser.Tokenizer;
import core.procedures.delayed.SCMPromise;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.errors.SCMError;
import core.scm.specialforms.SCMSpecialForm;
import org.junit.Test;

import java.util.Map;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EvaluatorTest {

  private final IParser tokenizer = new Tokenizer();
  private final IEvaluator eval = new Evaluator();
  private final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (Map.Entry<String, String> entry : env.getProcs().entrySet()) {
      env.put(entry.getKey(), eval.eval(tokenizer.parse(entry.getValue()), env));
    }
  }

  @Test
  public void testEvalNumbers() {
    assertEquals(1L, eval.eval(tokenizer.parse("1"), env));
    assertEquals(-15L, eval.eval(tokenizer.parse("-15"), env));
    assertEquals(-2.5d, eval.eval(tokenizer.parse("-2.5"), env));
  }

  @Test
  public void testEvalStrings() {
    assertEquals("1", eval.eval(tokenizer.parse("\"1\""), env));
    assertEquals("Lorem ipsum", eval.eval(tokenizer.parse("\"Lorem ipsum\""), env));
    assertEquals("Lorem \\\"ipsum\\\" ", eval.eval(tokenizer.parse("\"Lorem \\\"ipsum\\\" \""), env));
    assertEquals("", eval.eval(tokenizer.parse("\"\""), env));
  }

  @Test
  public void testEvalMath() {

    assertEquals(6L,  eval.eval(tokenizer.parse("(+ 1 2 3)"), env));
    assertEquals(5.5, eval.eval(tokenizer.parse("(/ (+ 1 2 3 (- (* 2 2.5 2) 5)) 2)"), env));
    assertEquals(5.0, eval.eval(tokenizer.parse("(/ 10.0 2)"), env));
    assertEquals(0.1, eval.eval(tokenizer.parse("(/ 10)"), env));
    assertEquals(3.25, eval.eval(tokenizer.parse("(/ 13 4)"), env));
    assertEquals(2L, eval.eval(tokenizer.parse("(/ 10 5)"), env));
    assertEquals(2d, eval.eval(tokenizer.parse("(/ 10.0 5)"), env));
    assertEquals(2d, eval.eval(tokenizer.parse("(/ 10 5.0)"), env));

    assertEquals(5L, eval.eval(tokenizer.parse("(abs 5)"), env));
    assertEquals(5L, eval.eval(tokenizer.parse("(abs -5)"), env));

    // abs
    try {
      eval.eval(tokenizer.parse("(abs)"), env);
    } catch (ArityException e) {
      assertTrue(e.getMessage().contains("Wrong number of arguments (0) passed to: abs"));
    }
    try {
      eval.eval(tokenizer.parse("(abs 1 2 3)"), env);
    } catch (ArityException e) {
      assertTrue(e.getMessage().contains("Wrong number of arguments (3) passed to: abs"));
    }
    try {
      eval.eval(tokenizer.parse("(abs \"not-a-number\")"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: Number, actual: String"));
    }

    // sqrt
    assertEquals(5d, eval.eval(tokenizer.parse("(sqrt 25)"), env));
    assertEquals(3d, eval.eval(tokenizer.parse("(sqrt 9.0)"), env));
    assertTrue(Double.isNaN((Double)eval.eval(tokenizer.parse("(sqrt -5)"), env)));

    // FIXME
//    assertEquals(0.001, eval.eval(tokenizer.parse("(/ 1 10 10)"), env));
  }

  @Test
  public void testNumberTheoreticDivision() {

    // quotient
    assertEquals(3L, eval.eval(tokenizer.parse("(quotient 13 4)"), env));
    assertEquals(3d, eval.eval(tokenizer.parse("(quotient 13.0 4)"), env));
    assertEquals(1L, eval.eval(tokenizer.parse("(quotient 5 5)"), env));
    assertEquals(1d, eval.eval(tokenizer.parse("(quotient 5.0 5)"), env));
    assertEquals(1d, eval.eval(tokenizer.parse("(quotient -5 -5.0)"), env));
    assertEquals(-1L, eval.eval(tokenizer.parse("(quotient -5 5)"), env));
    assertEquals(-1d, eval.eval(tokenizer.parse("(quotient -5 5.)"), env));
    try {
      eval.eval(tokenizer.parse("(quotient -10 0.0001)"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (quotient) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval.eval(tokenizer.parse("(quotient -10 0.0)"), env);
    } catch (ArithmeticException e) {
      assertEquals("/ by zero", e.getMessage());
    }

    // remainder
    assertEquals(-1L, eval.eval(tokenizer.parse("(remainder -13 4)"), env));
    assertEquals(1L, eval.eval(tokenizer.parse("(remainder 13 -4)"), env));
    assertEquals(-1L, eval.eval(tokenizer.parse("(remainder -13 -4)"), env));
    assertEquals(-1.0, eval.eval(tokenizer.parse("(remainder -13 -4.0)"), env));
    assertEquals(1L, eval.eval(tokenizer.parse("(remainder 13 4)"), env));
    assertEquals(0L, eval.eval(tokenizer.parse("(remainder 10 2)"), env));
    assertEquals(0d, eval.eval(tokenizer.parse("(remainder 10 2.0)"), env));
    assertEquals(0d, eval.eval(tokenizer.parse("(remainder -10 2.0)"), env));
    try {
      eval.eval(tokenizer.parse("(remainder -10 0.0001)"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (remainder) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval.eval(tokenizer.parse("(remainder -10 0.0)"), env);
    } catch (ArithmeticException e) {
      assertEquals("/ by zero", e.getMessage());
    }

    // modulo
    assertEquals(2L, eval.eval(tokenizer.parse("(modulo 5 3)"), env));
    assertEquals(2d, eval.eval(tokenizer.parse("(modulo 5 3.0)"), env));
    assertEquals(1L, eval.eval(tokenizer.parse("(modulo 13 4)"), env));
    assertEquals(-1L, eval.eval(tokenizer.parse("(modulo -13 -4)"), env));
    try {
      eval.eval(tokenizer.parse("(modulo -10 0.0001)"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (modulo) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval.eval(tokenizer.parse("(modulo -10 0.0)"), env);
    } catch (ArithmeticException e) {
      assertEquals("/ by zero", e.getMessage());
    }
    // FIXME
//    assertEquals(3L, eval.eval(tokenizer.parse("(modulo -13 4)"), env));
//    assertEquals(-3L, eval.eval(tokenizer.parse("(modulo 13 -4)"), env));
  }

  @Test
  public void testEvalLocalState() {

    IEnvironment lenv = new DefaultEnvironment();
    eval.eval(tokenizer.parse("(define (make-withdraw balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) \"Insufficient funds\")))"), lenv);

    eval.eval(tokenizer.parse("(define W1 (make-withdraw 100))"), lenv);
    eval.eval(tokenizer.parse("(define W2 (make-withdraw 100))"), lenv);
    assertEquals(50L, eval.eval(tokenizer.parse("(W1 50)"), lenv));
    assertEquals(30L, eval.eval(tokenizer.parse("(W2 70)"), lenv));
    assertEquals("Insufficient funds", eval.eval(tokenizer.parse("(W2 40)"), lenv));
    assertEquals(10L, eval.eval(tokenizer.parse("(W1 40)"), lenv));

    eval.eval(tokenizer.parse("(define a 999)"), lenv);
    assertEquals(5L, eval.eval(tokenizer.parse("(begin (define a 10) (set! a 5) a)"), lenv));
    assertEquals(20L, eval.eval(tokenizer.parse("(let ((a 5)) (set! a 10) (let () (set! a 15) (let () (+ a 5))))"), lenv));

    eval.eval(tokenizer.parse("(define x 2)"), lenv);
    eval.eval(tokenizer.parse("(define y 10)"), lenv);
    eval.eval(tokenizer.parse("(define multiply (lambda (x y) (* x y)))"), lenv);
    assertEquals(12L, eval.eval(tokenizer.parse("(+ x y)"), lenv));
    assertEquals(100L, eval.eval(tokenizer.parse("(multiply y 10)"), lenv));

    eval.eval(tokenizer.parse("(define x 10)"), lenv);
    eval.eval(tokenizer.parse("(set! x 20)"), lenv);
    assertEquals(20L, eval.eval(tokenizer.parse("x"), lenv));
    eval.eval(tokenizer.parse("(define add (lambda (x y) (set! x (+ x y)) x))"), lenv);
    assertEquals(110L, eval.eval(tokenizer.parse("(add 10 100)"), lenv));

    eval.eval(tokenizer.parse("(define x 4)"), lenv);
    eval.eval(tokenizer.parse("(define y 5)"), lenv);
    assertEquals(2L, eval.eval(tokenizer.parse("(let ((x 1) (y 2)) (* x y))"), lenv));
    assertEquals(20L, eval.eval(tokenizer.parse("(* x y)"), lenv));
  }

  @Test
  public void testEvalImplicitBegin() {

    assertEquals(3L, eval.eval(tokenizer.parse("((lambda () 1 2 (+ 1 2)))"), env));
    assertEquals(3L, eval.eval(tokenizer.parse("(let    () 1 2 (+ 1 2))"), env));
    assertEquals(3L, eval.eval(tokenizer.parse("(let*   () 1 2 (+ 1 2))"), env));
    assertEquals(3L, eval.eval(tokenizer.parse("(letrec () 1 2 (+ 1 2))"), env));

    eval.eval(tokenizer.parse("(define (a) 1 2 (+ 1 2))"), env);
    assertEquals(3L, eval.eval(tokenizer.parse("(a)"), env));
    // TODO do
    // TODO named-lambda
    // TODO fluid-let
  }

  @Test
  public void testEvalNumericalComparison() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(= 1 1 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(= 1 0 1)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(= 0)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(= 0.57 0.5700)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(= 7 7.00)"), env));

    assertEquals(TRUE,  eval.eval(tokenizer.parse("(> 2 1)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(> 2 1.123)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(>= 2 1.123)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(>= 2.5 1.123)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(<= -2.5 1.123)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(< -2.5 1.123)"), env));
  }

  @Test
  public void testEvalNegation() {
    assertEquals(FALSE, eval.eval(tokenizer.parse("(not #t)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(not #f)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(not (= 1 2 1))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(not (= 1 1 1))"), env));
  }

  // Equivalence
  @Test
  public void testEvalCharEq() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(char=? #\\A #\\A)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(char=? #\\B #\\A)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(char=? #\\newline #\\newline)"), env));
  }

  @Test
  public void testEvalCharEqCi() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(char-ci=? #\\Z #\\z)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(char-ci=? #\\b #\\A)"), env));
  }

  @Test
  public void testEvalStringEq() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(string=? \"test\" \"test\")"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(string=? \"test\" \"test123\")"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(string=? \"\" \"\")"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(string=? \"test\" \"Test\")"), env));
  }

  @Test
  public void testEvalStringEqCi() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(string-ci=? \"test\" \"test\")"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(string-ci=? \"test\" \"test123\")"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(string-ci=? \"\" \"\")"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(string-ci=? \"test\" \"Test\")"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(string-ci=? \"tESt\" \"TesT\")"), env));
  }

  @Test
  public void testEvalEq() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(eq? '() '())"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(eq? 1 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(eq? 1 2)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(eq? \"1\" \"1\")"), env));
  }

  @Test
  public void testEvalEqv() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(eqv? '() '())"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(eqv? 1 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(eqv? 1 2)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(eqv? \"1\" \"1\")"), env));
  }

  @Test
  public void testEvalEqual() {
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(equal? '() '())"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(equal? '(1 2 3) '( 1 2 3))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(equal? '(1 2 3 5) '( 1 2 3))"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(equal? 1 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(equal? 1 2)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(equal? \"1fe\" \"1fe\")"), env));
  }

  @Test
  public void testEvalDelayed() {

    assertEquals(1d, eval.eval(tokenizer.parse("(force (delay 1.0))"), env));
    assertEquals("test", eval.eval(tokenizer.parse("(force (delay \"test\"))"), env));
    assertEquals(10L, eval.eval(tokenizer.parse("(force (delay (+ 5 2 (* 1 3))))"), env));
    assertEquals(SCMPromise.class, eval.eval(tokenizer.parse("(delay 1.0)"), env).getClass());
    assertEquals(TRUE, eval.eval(tokenizer.parse("(promise? (delay 1.0))"), env));
    assertEquals(3L, eval.eval(tokenizer.parse("(force (delay (+ 1 2)))"), env));
    assertEquals(new SCMList(3L, 3L), eval.eval(tokenizer.parse("(let ((p (delay (+ 1 2))))(list (force p) (force p)))"), env));
  }

  @Test
  public void testEvalIsAChar() {
    assertEquals(TRUE, eval.eval(tokenizer.parse("(char? #\\A)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(char? \"A\")"), env));
  }

  @Test
  public void testEvalIsAString() {
    assertEquals(FALSE, eval.eval(tokenizer.parse("(string? #\\A)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(string? \"A\")"), env));
  }

  @Test
  public void testEvalIsAVector() {
    assertEquals(FALSE, eval.eval(tokenizer.parse("(vector? #\\A)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(vector? #(1 2 3 ))"), env));
  }

  @Test
  public void testEvalVector() {
    assertEquals(new SCMVector(), eval.eval(tokenizer.parse("#()"), env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval.eval(tokenizer.parse("#(1 2 3 )"), env));
  }

  @Test
  public void testEvalProcedure() {
    assertEquals(SCMProcedure.class, eval.eval(tokenizer.parse("(lambda () #t)"), env).getClass());
    assertEquals(TRUE, eval.eval(tokenizer.parse("((lambda () #t))"), env));
    assertEquals(6L, eval.eval(tokenizer.parse("((lambda (n) (+ n 1)) 5)"), env));

    eval.eval(tokenizer.parse("(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"), env);
    assertEquals(8L, eval.eval(tokenizer.parse("(fib 5)"), env));

    assertEquals(6L, eval.eval(tokenizer.parse("((lambda (n) (+ n 1)) 5)"), env));
  }

  @Test
  public void testEvalDefine() {
    eval.eval(tokenizer.parse("(define a 5)"), env);
    assertEquals(5L, eval.eval(tokenizer.parse("a"), env));
    assertEquals(SCMSpecialForm.UNSPECIFIED, eval.eval(tokenizer.parse("(define b 7)"), env));

    eval.eval(tokenizer.parse("(define edl (lambda (n) (+ n 1)))"), env);
    assertEquals(2L, eval.eval(tokenizer.parse("(edl 1)"), env));

    // variadic
    eval.eval(tokenizer.parse("(define edlv (lambda args args))"), env);
    assertEquals(new SCMList<Long>(1L, 2L, 3L, 4L, 5L), eval.eval(tokenizer.parse("(edlv 1 2 3 4 5)"), env));

    // variadic define
    eval.eval(tokenizer.parse("(define (edv1 first second . rest) rest)"), env);
    assertEquals(new SCMList<Long>(2L, 3L, 4L, 5L), eval.eval(tokenizer.parse("(edv1 0 1 2 3 4 5)"), env));

    eval.eval(tokenizer.parse("(define (edv2 first second . rest) second)"), env);
    assertEquals(1L, eval.eval(tokenizer.parse("(edv2 0 1 2 3 4 5)"), env));

    // internal define
    assertEquals(45L, eval.eval(tokenizer.parse("(let ((x 5))(define foo (lambda (y) (bar x y)))(define bar (lambda (a b) (+ (* a b) a)))(foo (+ x 3)))"), env));
    try {
      eval.eval(tokenizer.parse("(foo 5)"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Unbound variable: foo"));
    }
  }

  @Test
  public void testEvalLambda() {
    String f1 = "(lambda ())";
    try {
      eval.eval(tokenizer.parse(f1), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("bad lambda in form: " + f1));
    }
  }

  @Test
  public void testEvalIf() {
    assertEquals(5L, eval.eval(tokenizer.parse("(if #t 5 0)"), env));
    assertEquals(5L, eval.eval(tokenizer.parse("(if #f 0 5)"), env));
    assertEquals(0L, eval.eval(tokenizer.parse("(if (not #f) 0 5)"), env));
    assertEquals(5L, eval.eval(tokenizer.parse("(if (not (not (or #f #f))) 0 (+ 3 2))"), env));
    assertEquals(new SCMSymbol("yes"), eval.eval(tokenizer.parse("(if (> 3 2) 'yes 'no)"), env));
    assertEquals(new SCMSymbol("no"), eval.eval(tokenizer.parse("(if (> 2 3) 'yes 'no)"), env));
    assertEquals(1L, eval.eval(tokenizer.parse("(if (> 3 2)(- 3 2)(+ 3 2))"), env));
  }

  @Test
  public void testEvalWhen() {
    assertEquals(0L, eval.eval(tokenizer.parse("(when #t 5 4 3 2 1 0)"), env));
    assertEquals(null, eval.eval(tokenizer.parse("(when #f 5 4 3 2 1 0)"), env));
  }

  @Test
  public void testEvalQuote() {
    assertEquals(0L, eval.eval(tokenizer.parse("'0"), env));
    assertEquals("test", eval.eval(tokenizer.parse("'\"test\""), env));
    assertEquals(new SCMList<Object>(SCMSpecialForm.QUOTE, "test"), eval.eval(tokenizer.parse("''\"test\""), env));
    assertEquals(new SCMList<Object>(new SCMSymbol("+"), 1L, 2L), eval.eval(tokenizer.parse("'(+ 1 2)"), env));
  }

  @Test
  public void testEvalSet() {
    assertEquals(9L, eval.eval(tokenizer.parse("(let ((a 0)) (set! a 9) a)"), env));
    assertEquals(19L, eval.eval(tokenizer.parse("(begin (define a 0) (set! a 9) (+ a 10))"), env));
    try {
      eval.eval(tokenizer.parse("(begin (set! b 99) b)"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Unbound variable: b", e.getMessage());
    }
  }

  @Test
  public void testEvalLet() {
    assertEquals(124L, eval.eval(tokenizer.parse("(let ((c 123)) (+ c 1))"), env));
    assertEquals(555L, eval.eval(tokenizer.parse("(let ((c 123) (b 432)) (+ c b))"), env));
    try {
      eval.eval(tokenizer.parse("(let ((c 123) (c (+ 400 30 2))) (+ c b))"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("let: duplicate bound variable"));
    }
    try {
      eval.eval(tokenizer.parse("(let ((c 123))"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("let: bad let in form:"));
    }
    try {
      eval.eval(tokenizer.parse("(let ((z 1) (b (+ z 1))) b)"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Unbound variable: z"));
    }
  }

  @Test
  public void testEvalLetStar() {
    assertEquals(2L, eval.eval(tokenizer.parse("(let* ((z 1) (b (+ z 1))) b)"), env));
    try {
      eval.eval(tokenizer.parse("(let* ((c 123)))"), env);
    } catch (IllegalArgumentException e) {
      assertTrue("Test bad let* form", e.getMessage().contains("let*: bad let* in form:"));
    }
  }

  @Test
  public void testEvalLetRec() {
    String letrec1 = "(letrec ((is-even? (lambda (n) (or (= n 0) (is-odd? (- n 1))))) " +
        "(is-odd?  (lambda (n) (and (not (= n 0)) (is-even? (- n 1))))))" +
        "  (is-odd? 11))";
    assertEquals(TRUE, eval.eval(tokenizer.parse(letrec1), env));
  }

  @Test
  public void testEvalCond() {
    // "Source expression failed to match any pattern in form (cond)"
    try {
      eval.eval(tokenizer.parse("(cond)"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (cond)"));
    }
    // "Invalid clause in subform "
    try {
      eval.eval(tokenizer.parse("(cond 1)"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Invalid clause in subform 1"));
    }
    // "cond: else must be the last clause in subform"
    try {
      eval.eval(tokenizer.parse("(cond (else 1) (#t 5))"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("cond: else must be the last clause in subform"));
    }
    // "Source expression failed to match any pattern in form (cond)"
    try {
      eval.eval(tokenizer.parse("(cond)"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (cond)"));
    }

    assertEquals(1L, eval.eval(tokenizer.parse("(cond (#f 5) ((not #t) 7) (else 1))"), env));
    assertEquals(7L, eval.eval(tokenizer.parse("(cond (#f 5) ((not #f) 7) (else 1))"), env));

    assertEquals(new SCMSymbol("greater"), eval.eval(tokenizer.parse("(cond ((> 3 2) 'greater)((< 3 2) 'less))"), env));
    assertEquals(new SCMSymbol("equal"), eval.eval(tokenizer.parse("(cond ((> 3 3) 'greater)((< 3 3) 'less)(else 'equal))"), env));
  }

  @Test
  public void testEvalCase() {
    try {
      eval.eval(tokenizer.parse("(case)"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (case)"));
    }
    try {
      eval.eval(tokenizer.parse("(case 1 1)"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Invalid clause in subform 1"));
    }
    try {
      eval.eval(tokenizer.parse("(case (* 2 3) (else 'prime) ((1 4 6 8 9) 'composite))"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("case: else must be the last clause in subform"));
    }
    String caseform = "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))";
    assertEquals(new SCMSymbol("composite"), eval.eval(tokenizer.parse(caseform), env));

    caseform = "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 8 9) 'composite))";
    assertEquals(null, eval.eval(tokenizer.parse(caseform), env));

    caseform = "(case (* 2 3) ((2 3 5 7) 'prime) (else 'composite))";
    assertEquals(new SCMSymbol("composite"), eval.eval(tokenizer.parse(caseform), env));
  }

  @Test
  public void testEvalAnd() {
    assertEquals(TRUE, eval.eval(tokenizer.parse("(and)"), env));
    assertEquals(1L, eval.eval(tokenizer.parse("(and 1)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(and (= 2 2) (> 2 1))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(and (= 2 2) (< 2 1))"), env));
    assertEquals(new SCMList<Object>(new SCMSymbol("f"), new SCMSymbol("g")),
                 eval.eval(tokenizer.parse("(and 1 2 'c '(f g)) "), env));
  }

  @Test
  public void testEvalOr() {
    assertEquals(FALSE, eval.eval(tokenizer.parse("(or)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(or (= 2 2) (> 2 1)) "), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(or (= 2 2) (< 2 1))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(or #f #f #f)"), env));
    assertEquals(new SCMList<Object>(new SCMSymbol("f"), new SCMSymbol("g")),
                 eval.eval(tokenizer.parse("(or '(f g) 1 2)"), env));
  }

  @Test
  public void testEvalBegin() {
    try {
      eval.eval(tokenizer.parse("(begin (set! x 5) (+ x 1))"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Unbound variable: x", e.getMessage());
    }
    assertEquals(null, eval.eval(tokenizer.parse("(begin (display \"4 plus 1 equals \")(display (+ 4 1)))"), env));
  }

  @Test
  public void testEvalClassOf() {
    // class-of
    assertEquals(Long.class.getName(), eval.eval(tokenizer.parse("(class-of 1)"), env));
    assertEquals(Double.class.getName(), eval.eval(tokenizer.parse("(class-of 1.0)"), env));
    assertEquals(SCMPromise.class.getName(), eval.eval(tokenizer.parse("(class-of (delay 1))"), env));
  }

  @Test
  public void testEvalError() {
    // error
    try {
      eval.eval(tokenizer.parse("(error \"boom\")"), env).getClass();
    } catch (SCMError e) {
      assertTrue(e.getMessage().equals("ERROR:boom"));
    }
  }

  @Test
  public void testEvalList() {

    assertEquals(SCMList.class.getName(), eval.eval(tokenizer.parse("(class-of (list 1 2 3 4 5))"), env));
    assertEquals(new SCMList<Long>(1L, 2L, 3L), eval.eval(tokenizer.parse("(list 1 2 3)"), env));
  }

  @Test
  public void testEvalEmpty() {

    assertEquals(TRUE,  eval.eval(tokenizer.parse("(null?  '())"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(empty? '())"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(null?  '(1 2 3))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(empty? '(1 2 3))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(null?  1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(empty? 1)"), env));
  }

  @Test
  public void testEvalLength() {

    assertEquals(0L, eval.eval(tokenizer.parse("(length '())"), env));
    assertEquals(1L, eval.eval(tokenizer.parse("(length '(1))"), env));
    assertEquals(5L, eval.eval(tokenizer.parse("(length '(1 2 3 4 5))"), env));
  }

  @Test
  public void testEvalIsZero() {

    assertEquals(TRUE,  eval.eval(tokenizer.parse("(zero? 0)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(zero? 0.0)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(zero? 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(zero? -5)"), env));

    try {
      eval.eval(tokenizer.parse("(zero? \"test\")"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalNegative() {

    assertEquals(FALSE, eval.eval(tokenizer.parse("(negative? 0)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(negative? 0.0)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(negative? 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(negative? (* -5 -6))"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(negative? -5)"), env));
    try {
      eval.eval(tokenizer.parse("(negative? \"test\")"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalPositive() {

    assertEquals(FALSE, eval.eval(tokenizer.parse("(positive? 0)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(positive? 0.0)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(positive? 1)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(positive? (* -5 -6))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(positive? -5)"), env));
    try {
      eval.eval(tokenizer.parse("(positive? \"test\")"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalEven() {

    assertEquals(TRUE,  eval.eval(tokenizer.parse("(even? 0)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(even? 0.0)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(even? 4)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(even? 100)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(even? 1)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(even? (* -5 -6))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(even? -5)"), env));
    try {
      eval.eval(tokenizer.parse("(even? \"test\")"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalOdd() {

    assertEquals(FALSE, eval.eval(tokenizer.parse("(odd? 0)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(odd? 0.0)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(odd? 4)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(odd? 100)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(odd? 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(odd? 4)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(odd? (* -5 -6))"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(odd? -5)"), env));
    try {
      eval.eval(tokenizer.parse("(odd? \"test\")"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalRound() {

    assertEquals(0L,   eval.eval(tokenizer.parse("(round 0)"),    env));
    assertEquals(4L,   eval.eval(tokenizer.parse("(round 4)"),    env));
    assertEquals(-4L,  eval.eval(tokenizer.parse("(round -4)"),   env));
    assertEquals(0.0,  eval.eval(tokenizer.parse("(round 0.0)"),  env));
    assertEquals(1.0,  eval.eval(tokenizer.parse("(round 1.0)"),  env));
    assertEquals(2.0,  eval.eval(tokenizer.parse("(round 1.5)"),  env));
    assertEquals(-2.0, eval.eval(tokenizer.parse("(round -1.5)"), env));
    assertEquals(2.0,  eval.eval(tokenizer.parse("(round 2.5)"),  env));
    assertEquals(-0.0, eval.eval(tokenizer.parse("(round -0.5)"), env));
    assertEquals(-2.0, eval.eval(tokenizer.parse("(round -1.7)"), env));
    assertEquals(4.0,  eval.eval(tokenizer.parse("(round 3.7)"),  env));
    assertEquals(3.0,  eval.eval(tokenizer.parse("(round 2.7)"),  env));
    assertEquals(2.0,  eval.eval(tokenizer.parse("(round 2.5)"),  env));
  }

  @Test
  public void testEvalIsInteger() {

    assertEquals(TRUE,  eval.eval(tokenizer.parse("(integer? 0)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(integer? 0.0)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(integer? 4)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(integer? 100)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(integer? 1)"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(integer? (* -5 -6))"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(integer? -5)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(integer? -5.4)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(integer? 3.14)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(integer? .123)"), env));
    try {
      eval.eval(tokenizer.parse("(integer? \"test\")"), env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalNamedLet() {

    assertEquals(120L, eval.eval(tokenizer.parse("(let fact ((n 5) (acc 1)) (if (= n 0) acc (fact (- n 1) (* acc n))))"), env));
    assertEquals(12L,  eval.eval(tokenizer.parse("(let t ((x 5) (y 7)) (+ x y))"), env));

    try {
      eval.eval(tokenizer.parse("(let fact ((n 5) (n 1)) (if (= n 0) acc (fact (- n 1) (* n n))))"), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("let: duplicate bound variable: n"));
    }
  }
  // TODO Exceptions
}
