import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.parser.IParser;
import core.parser.Tokenizer;
import core.procedures.delayed.SCMPromise;
import core.procedures.io.Display;
import core.procedures.io.Newline;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.errors.SCMError;
import core.scm.specialforms.SCMSpecialForm;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.util.Map;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.specialforms.SCMSpecialForm.UNSPECIFIED;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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
      fail();
    } catch (ArityException e) {
      assertTrue(e.getMessage().contains("Wrong number of arguments (actual: 0, expected: 1) passed to: abs"));
    }
    try {
      eval.eval(tokenizer.parse("(abs 1 2 3)"), env);
      fail();
    } catch (ArityException e) {
      assertTrue(e.getMessage().contains("Wrong number of arguments (actual: 3, expected: 1) passed to: abs"));
    }
    try {
      eval.eval(tokenizer.parse("(abs \"not-a-number\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: Number, actual: String"));
    }

    // sqrt
    assertEquals(5d, eval.eval(tokenizer.parse("(sqrt 25)"), env));
    assertEquals(3d, eval.eval(tokenizer.parse("(sqrt 9.0)"), env));
    assertTrue(Double.isNaN((Double)eval.eval(tokenizer.parse("(sqrt -5)"), env)));

    assertEquals(0.01, eval.eval(tokenizer.parse("(/ 1 10 10)"), env));
  }

  @Test
  public void testNumberTheoreticDivision() {

    // quotient
    assertEquals(3L,  eval.eval(tokenizer.parse("(quotient 13 4)"), env));
    assertEquals(3d,  eval.eval(tokenizer.parse("(quotient 13.0 4)"), env));
    assertEquals(1L,  eval.eval(tokenizer.parse("(quotient 5 5)"), env));
    assertEquals(1d,  eval.eval(tokenizer.parse("(quotient 5.0 5)"), env));
    assertEquals(1d,  eval.eval(tokenizer.parse("(quotient -5 -5.0)"), env));
    assertEquals(-1L, eval.eval(tokenizer.parse("(quotient -5 5)"), env));
    assertEquals(-1d, eval.eval(tokenizer.parse("(quotient -5 5.)"), env));
    try {
      eval.eval(tokenizer.parse("(quotient -10 0.0001)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (quotient) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval.eval(tokenizer.parse("(quotient -10 0.0)"), env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("Error: (quotient) undefined for 0", e.getMessage());
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
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (remainder) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval.eval(tokenizer.parse("(remainder -10 0.0)"), env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("Error: (remainder) undefined for 0", e.getMessage());
    }

    // modulo
    assertEquals(2L,  eval.eval(tokenizer.parse("(modulo 5 3)"), env));
    assertEquals(2d,  eval.eval(tokenizer.parse("(modulo 5 3.0)"), env));
    assertEquals(1L,  eval.eval(tokenizer.parse("(modulo 13 4)"), env));
    assertEquals(-1L, eval.eval(tokenizer.parse("(modulo -13 -4)"), env));
    try {
      eval.eval(tokenizer.parse("(modulo -10 0.0001)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (modulo) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval.eval(tokenizer.parse("(modulo -10 0.0)"), env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("Error: (modulo) undefined for 0", e.getMessage());
    }
    assertEquals(3L,  eval.eval(tokenizer.parse("(modulo -13 4)"), env));
    assertEquals(-3L, eval.eval(tokenizer.parse("(modulo 13 -4)"), env));
  }

  @Test
  public void testEvalMutualRecursion() {

    String f = "(define (F n) (if (= n 0) 1 (- n (M (F (- n 1))))))";
    String m = "(define (M n) (if (= n 0) 0 (- n (F (M (- n 1))))))";
    eval.eval(tokenizer.parse(f), env);
    eval.eval(tokenizer.parse(m), env);

    long[] fs = {1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13};
    for (int i = 0; i < fs.length; i++) {
      assertEquals(fs[i], eval.eval(tokenizer.parse(String.format("(F %s)", i)), env));
    }

    long[] ms = {0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9, 9, 10, 11, 11, 12, 12};
    for (int i = 0; i < ms.length; i++) {
      assertEquals(ms[i], eval.eval(tokenizer.parse(String.format("(M %s)", i)), env));
    }

    String letrec = "(letrec ((F (lambda (n) (if (= n 0) 1 (- n (M (F (- n 1)))))))" +
                             "(M (lambda (n) (if (= n 0) 0 (- n (F (M (- n 1))))))))" +
                      "(F 19))";
    assertEquals(12L, eval.eval(tokenizer.parse(letrec), env));
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

    assertEquals(new SCMVector(), eval.eval(tokenizer.parse("(vector)"), env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval.eval(tokenizer.parse("(vector 1 2 3)"), env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval.eval(tokenizer.parse("(vector 1 2 (+ 1 2))"), env));
  }

  @Test
  public void testEvalMakeVector() {
    assertEquals(new SCMVector(1L, 1L, 1L), eval.eval(tokenizer.parse("(make-vector 3 1)"), env));
    assertEquals(new SCMVector(), eval.eval(tokenizer.parse("(make-vector 0)"), env));
    assertEquals(new SCMVector(UNSPECIFIED, UNSPECIFIED, UNSPECIFIED), eval.eval(tokenizer.parse("(make-vector 3)"), env));
    try {
      eval.eval(tokenizer.parse("(make-vector 1 2 3)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong number of arguments to `make-vector'"));
    }

    try {
      eval.eval(tokenizer.parse("(make-vector \"test\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: String"));
    }
  }

  @Test
  public void testEvalVectorLength() {
    assertEquals(0L, eval.eval(tokenizer.parse("(vector-length #())"), env));
    assertEquals(0L, eval.eval(tokenizer.parse("(vector-length (vector))"), env));
    assertEquals(3L, eval.eval(tokenizer.parse("(vector-length (vector 1 2 3))"), env));

    try {
      eval.eval(tokenizer.parse("(vector-length 1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: Long"));
    }
  }

  @Test
  public void testEvalVectorRef() {
    assertEquals(1L, eval.eval(tokenizer.parse("(vector-ref (vector 1 2 3) 0)"), env));
    assertEquals(2L, eval.eval(tokenizer.parse("(vector-ref (vector 1 2 3) 1)"), env));
    assertEquals(3L, eval.eval(tokenizer.parse("(vector-ref (vector 1 2 3) 2)"), env));
    assertEquals("test", eval.eval(tokenizer.parse("(vector-ref (vector \"test\" 2 3) 0)"), env));

    try {
      eval.eval(tokenizer.parse("(vector-ref (vector 1 2 3) -1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }
    try {
      eval.eval(tokenizer.parse("(vector-ref (vector 1 2 3) 3)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }
    try {
      eval.eval(tokenizer.parse("(vector-ref (vector) 0)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }
    try {
      eval.eval(tokenizer.parse("(vector-ref '(1 2 3) 0)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: SCMList"));
    }
    try {
      eval.eval(tokenizer.parse("(vector-ref (vector 1 2 3) 0.5)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: Double"));
    }
  }

  @Test
  public void testEvalVectorSet() {

    String sexp = "(begin (define v (vector 1 2 3))" +
                  "       (vector-set! v 0 99)" +
                  "       (vector-ref  v 0))";
    assertEquals(99L, eval.eval(tokenizer.parse(sexp), env));

    sexp = "(begin (define v (vector 1 2 3))" +
           "       (vector-set! v 2 \"test\")" +
           "       (vector-ref  v 2))";
    assertEquals("test", eval.eval(tokenizer.parse(sexp), env));

    sexp = "(begin (define v (vector 1 2 3))" +
           "       (vector-set! v -1 \"test\"))";
    try {
      eval.eval(tokenizer.parse(sexp), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }

    sexp = "(begin (define v (vector 1 2 3))" +
           "       (vector-set! v 3 \"test\"))";
    try {
      eval.eval(tokenizer.parse(sexp), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }

    sexp = "(begin (define v (vector))" +
           "       (vector-set! v 0 \"test\"))";
    try {
      eval.eval(tokenizer.parse(sexp), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }

    sexp = "(begin (define v '(1 2 3))" +
           "       (vector-set! v 0 \"test\"))";
    try {
      eval.eval(tokenizer.parse(sexp), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: SCMList"));
    }

    sexp = "(begin (define v (vector 1 2))" +
           "       (vector-set! v 0.5 \"test\"))";
    try {
      eval.eval(tokenizer.parse(sexp), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: Double"));
    }
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
    assertEquals(UNSPECIFIED, eval.eval(tokenizer.parse("(define b 7)"), env));

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
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Unbound variable: foo"));
    }
  }

  @Test
  public void testEvalQuine() {

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintStream old = System.out;
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (Map.Entry<String, String> entry : ((DefaultEnvironment)tempEnv).getProcs().entrySet()) {
      tempEnv.put(entry.getKey(), eval.eval(tokenizer.parse(entry.getValue()), tempEnv));
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));

    String quine = "((lambda (s) (display (list s (list (quote quote) s))))" +
                   " (quote (lambda (s) (display (list s (list (quote quote) s))))))";
    eval.eval(tokenizer.parse(quine), tempEnv);
    assertEquals(quine, baos.toString().trim());

    System.setOut(old);
  }

  @Test
  public void testEvalLambda() {
    String f1 = "(lambda ())";
    try {
      eval.eval(tokenizer.parse(f1), env);
      fail();
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
      fail();
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
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("let: duplicate bound variable"));
    }
    try {
      eval.eval(tokenizer.parse("(let ((c 123))"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("let: bad let in form:"));
    }
    try {
      eval.eval(tokenizer.parse("(let ((z 1) (b (+ z 1))) b)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Unbound variable: z"));
    }
  }

  @Test
  public void testEvalLetStar() {
    assertEquals(2L, eval.eval(tokenizer.parse("(let* ((z 1) (b (+ z 1))) b)"), env));
    try {
      eval.eval(tokenizer.parse("(let* ((c 123)))"), env);
      fail();
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
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (cond)"));
    }
    // "Invalid clause in subform "
    try {
      eval.eval(tokenizer.parse("(cond 1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Invalid clause in subform 1"));
    }
    // "cond: else must be the last clause in subform"
    try {
      eval.eval(tokenizer.parse("(cond (else 1) (#t 5))"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("cond: else must be the last clause in subform"));
    }
    // "Source expression failed to match any pattern in form (cond)"
    try {
      eval.eval(tokenizer.parse("(cond)"), env);
      fail();
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
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (case)"));
    }
    try {
      eval.eval(tokenizer.parse("(case 1 1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Invalid clause in subform 1"));
    }
    try {
      eval.eval(tokenizer.parse("(case (* 2 3) (else 'prime) ((1 4 6 8 9) 'composite))"), env);
      fail();
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
      fail();
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
      fail();
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
      fail();
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
      fail();
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
      fail();
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
      fail();
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
      fail();
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
    try {
      eval.eval(tokenizer.parse("(round \"test\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalFloor() {

    assertEquals(0L,   eval.eval(tokenizer.parse("(floor 0)"),    env));
    assertEquals(4L,   eval.eval(tokenizer.parse("(floor 4)"),    env));
    assertEquals(-5.0, eval.eval(tokenizer.parse("(floor -4.3)"), env));
    assertEquals(3.0,  eval.eval(tokenizer.parse("(floor 3.5)"),  env));
    assertEquals(1.0,  eval.eval(tokenizer.parse("(floor 1.2)"),  env));
    assertEquals(-2.0, eval.eval(tokenizer.parse("(floor -1.2)"), env));
    try {
      eval.eval(tokenizer.parse("(floor \"test\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalCeiling() {

    assertEquals(0L,   eval.eval(tokenizer.parse("(ceiling 0)"),    env));
    assertEquals(4L,   eval.eval(tokenizer.parse("(ceiling 4)"),    env));
    assertEquals(-4.0, eval.eval(tokenizer.parse("(ceiling -4.3)"), env));
    assertEquals(4.0,  eval.eval(tokenizer.parse("(ceiling 3.5)"),  env));
    assertEquals(2.0,  eval.eval(tokenizer.parse("(ceiling 1.2)"),  env));
    assertEquals(-1.0, eval.eval(tokenizer.parse("(ceiling -1.2)"), env));
    try {
      eval.eval(tokenizer.parse("(ceiling \"test\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalTruncate() {

    assertEquals(0L,   eval.eval(tokenizer.parse("(truncate 0)"),    env));
    assertEquals(4L,   eval.eval(tokenizer.parse("(truncate 4)"),    env));
    assertEquals(-4L,  eval.eval(tokenizer.parse("(truncate -4)"),   env));
    assertEquals(3.0,  eval.eval(tokenizer.parse("(truncate 3.5)"),  env));
    assertEquals(-3.0, eval.eval(tokenizer.parse("(truncate -3.5)"), env));
    assertEquals(2.0,  eval.eval(tokenizer.parse("(truncate 2.2)"),  env));
    assertEquals(-1.0, eval.eval(tokenizer.parse("(truncate -1.2)"), env));
    try {
      eval.eval(tokenizer.parse("(truncate \"test\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalMax() {

    assertEquals(0L,   eval.eval(tokenizer.parse("(max 0)"),    env));
    assertEquals(5.0,  eval.eval(tokenizer.parse("(max 5.0)"),  env));
    assertEquals(-5.0, eval.eval(tokenizer.parse("(max -5.0)"), env));

    assertEquals(-5.0, eval.eval(tokenizer.parse("(max -6 -7 -5.0)"), env));
    assertEquals(7.0,  eval.eval(tokenizer.parse("(max 6 7 5.0)"),    env));

    try {
      eval.eval(tokenizer.parse("(max \"test\" 1 2 3)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }

    try {
      eval.eval(tokenizer.parse("(max 0 \"test\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalMin() {

    assertEquals(0L,   eval.eval(tokenizer.parse("(min 0)"),    env));
    assertEquals(5.0,  eval.eval(tokenizer.parse("(min 5.0)"),  env));
    assertEquals(-5.0, eval.eval(tokenizer.parse("(min -5.0)"), env));

    assertEquals(-7.0, eval.eval(tokenizer.parse("(min -6 -7 -5.0)"), env));
    assertEquals(5.0,  eval.eval(tokenizer.parse("(min 6 7 5.0)"),    env));

    try {
      eval.eval(tokenizer.parse("(min \"test\" 1 2 3)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }

    try {
      eval.eval(tokenizer.parse("(min 0 \"test\")"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testEvalExpt() {

    assertEquals(1.0, eval.eval(tokenizer.parse("(expt 9 0)"), env));
    assertEquals(0.0, eval.eval(tokenizer.parse("(expt 0 10)"), env));
    assertEquals(1.0, eval.eval(tokenizer.parse("(expt 1 1)"), env));
    assertEquals(8.0, eval.eval(tokenizer.parse("(expt 2 3)"), env));
    assertEquals(16777216.0, eval.eval(tokenizer.parse("(expt 4 12)"), env));
    assertEquals(25.0, eval.eval(tokenizer.parse("(expt -5 2)"), env));
    assertEquals(-125.0, eval.eval(tokenizer.parse("(expt -5 3)"), env));
    assertEquals(13.489468760533386, eval.eval(tokenizer.parse("(expt 2.2 3.3)"), env));
    try {
      eval.eval(tokenizer.parse("(expt \"test\" 1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
    try {
      eval.eval(tokenizer.parse("(expt 1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong number of arguments (actual: 1, expected: 2) passed to: expt", e.getMessage());
    }
  }

  @Test
  public void testBigDecimal() {

    String big0 = "2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
    assertEquals(new BigDecimal(big0), eval.eval(tokenizer.parse(big0), env));

    BigDecimal big1 = new BigDecimal("46253220748483971668312221557921994865355615937726643119142534763573384386717010179" +
        "33035570379376866259256597659610145236756255853523897837700465755231009606767424706" +
        "73348011864143704094117912896787287887428473700404535710995567015427340013704721113" +
        "33316343260021388334157118647272603383652782933091786683436259682248241271823609303" +
        "74088996645418723950501545025386234033486857262740020740808886229945286599837304752" +
        "25837944037056491016035642078654527374207462210630264065442615967117912099739418879" +
        "84132513");

    assertEquals(big1, eval.eval(tokenizer.parse("(expt 33 333)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse(String.format("(number? %s)", big0)), env));

    assertEquals(new BigDecimal(big0), eval.eval(tokenizer.parse(String.format("(* (/ %s 10) 10)", big0)), env));
    assertEquals(new BigDecimal(big0).multiply(new BigDecimal(2)), eval.eval(tokenizer.parse(String.format("(+ %s %s)", big0, big0)), env));
    assertEquals(new BigDecimal(big0).multiply(new BigDecimal(2)).subtract(new BigDecimal(big0)),
                 eval.eval(tokenizer.parse(String.format("(- (* 2 %s) %s)", big0, big0)), env));


    assertEquals(new BigDecimal(big0), eval.eval(tokenizer.parse(String.format("(truncate (+ 0.2 %s))", big0)),  env));
    assertEquals(new BigDecimal(big0).negate(), eval.eval(tokenizer.parse(String.format("(truncate (+ 0.2 -%s))", big0)),  env));
    assertEquals(new BigDecimal(big0), eval.eval(tokenizer.parse(String.format("(floor (+ 0.2 %s))", big0)),  env));
    assertEquals(new BigDecimal(big0).add(BigDecimal.ONE),
                 eval.eval(tokenizer.parse(String.format("(ceiling (+ 0.2 %s))", big0)),  env));

    assertEquals(new BigDecimal(big0), eval.eval(tokenizer.parse(String.format("(abs -%s)", big0)),  env));
    assertEquals(new BigDecimal(big0).add(BigDecimal.ONE),
                 eval.eval(tokenizer.parse(String.format("(max (+ 1 %s) %s)", big0, big0)),  env));

    assertEquals(new BigDecimal(big0),
                 eval.eval(tokenizer.parse(String.format("(min (+ 1 %s) %s)", big0, big0)),  env));

    String big2 = "941737268473075634481294063531333847658485002458168527101639838005582185517473483816983389228732066437165294377295109210176795859047876399460771530181828861843994801526320659067260600443063376955200810073997787724454002350759571876705644517946943898492214066331998886559185229835330687165577365519449395424366904222913306696961330084086377946063169138303897697242206192836209273444873251023411764271944704088313845446589768727760791185170266144604537045173629663045739300767985189493967771010336173962367396474652866334212802605674879313278209206179544726008444885447395757991883875945457869103573901612777316112247438629624081718143710269108788904389008167209091151002216893051746019091645742839251513268837094248809018521046734530253606053753445604156050903737280600427015788467630468023527367174845920094011539693975275654700093627716413";
    assertEquals(BigDecimal.ONE, eval.eval(tokenizer.parse(String.format("(modulo %s 4)", big2)), env));
    assertEquals(new BigDecimal("-2"), eval.eval(tokenizer.parse(String.format("(modulo %s -5)", big2)), env));

    assertEquals(BigDecimal.ONE, eval.eval(tokenizer.parse(String.format("(remainder %s 4)", big2)), env));
    assertEquals(new BigDecimal("3"), eval.eval(tokenizer.parse(String.format("(remainder %s -5)", big2)), env));

    String quotientResult1 = "470868634236537817240647031765666923829242501229084263550819919002791092758736741908491694614366033218582647188647554605088397929523938199730385765090914430921997400763160329533630300221531688477600405036998893862227001175379785938352822258973471949246107033165999443279592614917665343582788682759724697712183452111456653348480665042043188973031584569151948848621103096418104636722436625511705882135972352044156922723294884363880395592585133072302268522586814831522869650383992594746983885505168086981183698237326433167106401302837439656639104603089772363004222442723697878995941937972728934551786950806388658056123719314812040859071855134554394452194504083604545575501108446525873009545822871419625756634418547124404509260523367265126803026876722802078025451868640300213507894233815234011763683587422960047005769846987637827350046813858206";
    assertEquals(new BigDecimal(quotientResult1), eval.eval(tokenizer.parse(String.format("(quotient %s 2)", big2)), env));
    assertEquals(new BigDecimal(2), eval.eval(tokenizer.parse(String.format("(quotient %s (quotient %s 2))", big2, big2)), env));

    assertEquals(TRUE, eval.eval(tokenizer.parse(String.format("(eqv? %s %s)", big2, big2)), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse(String.format("(<= %s %s)", big2, big2)), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse(String.format("(< %s %s)", big2, big2)), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse(String.format("(> (+ 1 %s) %s)", big2, big2)), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse(String.format("(< (+ 1 2) %s)", big2)), env));

    // TODO fix?
    assertEquals(Double.POSITIVE_INFINITY, eval.eval(tokenizer.parse(String.format("(sqrt %s)", big2)), env));
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
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: String", e.getMessage());
    }
  }

  @Test
  public void testIntegerRoots() {
    String code = "(define (root a b)" +
                    "(define // quotient)" +
                    "(define (y a a1 b c d e)" +
                    "(if (or (= c d) (= c e))" +
                    "(min d e)" +
                    "(y a a1 b d e (// (+ (* a1 e)" +
                    "(// b (expt e a1))) a))))" +
                    "(if (< b 2)" +
                    "b" +
                    "(let* ((a1 (- a 1))" +
                           "(c 1)" +
                           "(d (// (+ (* a1 c) (// b (expt c a1))) a))" +
                           "(e (// (+ (* a1 d) (// b (expt d a1))) a)))" +
                             "(y a a1 b c d e))))";

    IEnvironment tempEnv = new DefaultEnvironment();
    eval.eval(tokenizer.parse(code), tempEnv);
    assertEquals(2.0, eval.eval(tokenizer.parse("(root 3 8)"), tempEnv));
    // FIXME
//    assertEquals(2.0, eval.eval(tokenizer.parse("(root 3 (* 2 (expt 1000 2000)))"), tempEnv));
  }

  @Test
  public void testEvalHanoi() {

    PrintStream old = System.out;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (Map.Entry<String, String> entry : ((DefaultEnvironment)tempEnv).getProcs().entrySet()) {
      tempEnv.put(entry.getKey(), eval.eval(tokenizer.parse(entry.getValue()), tempEnv));
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));
    tempEnv.put(new SCMSymbol("newline"), new Newline(System.out));

    String hanoi = "(define (hanoi n a b c) (if (> n 0) (begin (hanoi (- n 1) a c b) (display \"Move disk from pole \") (display a) (display \" to pole \") (display b) (newline) (hanoi (- n 1) c b a)) #t))";
    eval.eval(tokenizer.parse(hanoi), tempEnv);
    eval.eval(tokenizer.parse("(hanoi 4 1 2 3)"), tempEnv);

    String solution = "Move disk from pole 1 to pole 3\n" +
                      "Move disk from pole 1 to pole 2\n" +
                      "Move disk from pole 3 to pole 2\n" +
                      "Move disk from pole 1 to pole 3\n" +
                      "Move disk from pole 2 to pole 1\n" +
                      "Move disk from pole 2 to pole 3\n" +
                      "Move disk from pole 1 to pole 3\n" +
                      "Move disk from pole 1 to pole 2\n" +
                      "Move disk from pole 3 to pole 2\n" +
                      "Move disk from pole 3 to pole 1\n" +
                      "Move disk from pole 2 to pole 1\n" +
                      "Move disk from pole 3 to pole 2\n" +
                      "Move disk from pole 1 to pole 3\n" +
                      "Move disk from pole 1 to pole 2\n" +
                      "Move disk from pole 3 to pole 2";

    assertEquals(solution, baos.toString().trim());
    System.setOut(old);
  }

  @Test
  public void testEvalSymbolStringConversion() {

    assertEquals("test", eval.eval(tokenizer.parse("(symbol->string 'test)"), env));
    assertEquals("test", eval.eval(tokenizer.parse("(symbol->string (string->symbol (symbol->string 'test)))"), env));
    assertEquals(new SCMSymbol("test"), eval.eval(tokenizer.parse("(string->symbol (symbol->string 'test))"), env));

    try {
      eval.eval(tokenizer.parse("(symbol->string 1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: Symbol, actual: Long"));
    }
    try {
      eval.eval(tokenizer.parse("(string->symbol 1)"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: String, actual: Long"));
    }
  }

  @Test
  public void testEvalNamedLet() {

    assertEquals(120L, eval.eval(tokenizer.parse("(let fact ((n 5) (acc 1)) (if (= n 0) acc (fact (- n 1) (* acc n))))"), env));
    assertEquals(12L,  eval.eval(tokenizer.parse("(let t ((x 5) (y 7)) (+ x y))"), env));

    try {
      eval.eval(tokenizer.parse("(let fact ((n 5) (n 1)) (if (= n 0) acc (fact (- n 1) (* n n))))"), env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("let: duplicate bound variable: n"));
    }
  }
  // TODO Exceptions
}
