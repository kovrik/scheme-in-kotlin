import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.WrongTypeException;
import core.procedures.delayed.SCMPromise;
import core.procedures.io.Display;
import core.procedures.io.Newline;
import core.reader.IReader;
import core.reader.Reader;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.errors.SCMError;
import core.scm.specialforms.SCMSpecialForm;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.util.Calendar;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.*;
import static core.scm.specialforms.SCMSpecialForm.UNSPECIFIED;
import static org.junit.Assert.*;

public class EvaluatorTest {

  private final IReader reader = new Reader();
  private final IEvaluator eval = new Evaluator();
  private final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (String proc : env.getLibraryProcedures()) {
      eval(proc, env);
    }
  }

  @Before
  public void setUp() throws Exception {
    // TODO Create new environment for each test?
  }

  @Test
  public void testEvalNumbers() {
    assertEquals(1L, eval("1", env));
    assertEquals(-15L, eval("-15", env));
    assertEquals(-2.5d, eval("-2.5", env));
  }

  @Test
  public void testEvalStrings() {
    assertEquals("1", eval("\"1\"", env));
    assertEquals("Lorem ipsum", eval("\"Lorem ipsum\"", env));
    assertEquals("Lorem \\\"ipsum\\\" ", eval("\"Lorem \\\"ipsum\\\" \"", env));
    assertEquals("", eval("\"\"", env));
  }

  @Test
  public void testEvalMath() {
    assertEquals(6L,  eval("(+ 1 2 3)", env));
    assertEquals(5.5, eval("(/ (+ 1 2 3 (- (* 2 2.5 2) 5)) 2)", env));
    assertEquals(5.0, eval("(/ 10.0 2)", env));
    assertEquals(0.1, eval("(/ 10)", env));
    assertEquals(3.25, eval("(/ 13 4)", env));
    assertEquals(2L, eval("(/ 10 5)", env));
    assertEquals(2d, eval("(/ 10.0 5)", env));
    assertEquals(2d, eval("(/ 10 5.0)", env));

    assertEquals(5L, eval("(abs 5)", env));
    assertEquals(5L, eval("(abs -5)", env));

    // abs
    try {
      eval("(abs)", env);
      fail();
    } catch (ArityException e) {
      assertTrue(e.getMessage().contains("Wrong number of arguments (actual: 0, expected: 1) passed to: abs"));
    }
    try {
      eval("(abs 1 2 3)", env);
      fail();
    } catch (ArityException e) {
      assertTrue(e.getMessage().contains("Wrong number of arguments (actual: 3, expected: 1) passed to: abs"));
    }
    try {
      eval("(abs \"not-a-number\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: Number, actual: \"not-a-number\""));
    }

    // sqrt
    assertEquals(5d, eval("(sqrt 25)", env));
    assertEquals(3d, eval("(sqrt 9.0)", env));
    assertTrue(Double.isNaN((Double)eval("(sqrt -5)", env)));

    assertEquals(0.01, eval("(/ 1 10 10)", env));
  }

  @Test
  public void testNumberTheoreticDivision() {
    // quotient
    assertEquals(3L,  eval("(quotient 13 4)", env));
    assertEquals(3d,  eval("(quotient 13.0 4)", env));
    assertEquals(1L,  eval("(quotient 5 5)", env));
    assertEquals(1d,  eval("(quotient 5.0 5)", env));
    assertEquals(1d,  eval("(quotient -5 -5.0)", env));
    assertEquals(-1L, eval("(quotient -5 5)", env));
    assertEquals(-1d, eval("(quotient -5 5.)", env));
    try {
      eval("(quotient -10 0.0001)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (quotient) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval("(quotient -10 0.0)", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("Error: (quotient) undefined for 0", e.getMessage());
    }

    // remainder
    assertEquals(-1L, eval("(remainder -13 4)", env));
    assertEquals(1L, eval("(remainder 13 -4)", env));
    assertEquals(-1L, eval("(remainder -13 -4)", env));
    assertEquals(-1.0, eval("(remainder -13 -4.0)", env));
    assertEquals(1L, eval("(remainder 13 4)", env));
    assertEquals(0L, eval("(remainder 10 2)", env));
    assertEquals(0d, eval("(remainder 10 2.0)", env));
    assertEquals(0d, eval("(remainder -10 2.0)", env));
    try {
      eval("(remainder -10 0.0001)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (remainder) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval("(remainder -10 0.0)", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("Error: (remainder) undefined for 0", e.getMessage());
    }

    // modulo
    assertEquals(2L,  eval("(modulo 5 3)", env));
    assertEquals(2d,  eval("(modulo 5 3.0)", env));
    assertEquals(1L,  eval("(modulo 13 4)", env));
    assertEquals(-1L, eval("(modulo -13 -4)", env));
    try {
      eval("(modulo -10 0.0001)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Error: (modulo) bad argument type - not an integer: 1.0E-4", e.getMessage());
    }
    try {
      eval("(modulo -10 0.0)", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("Error: (modulo) undefined for 0", e.getMessage());
    }
    assertEquals(3L,  eval("(modulo -13 4)", env));
    assertEquals(-3L, eval("(modulo 13 -4)", env));
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
  public void testEvalLocalState() {

    IEnvironment lenv = new DefaultEnvironment();
    eval("(define (make-withdraw balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) \"Insufficient funds\")))", lenv);

    eval("(define W1 (make-withdraw 100))", lenv);
    eval("(define W2 (make-withdraw 100))", lenv);
    assertEquals(50L, eval("(W1 50)", lenv));
    assertEquals(30L, eval("(W2 70)", lenv));
    assertEquals("Insufficient funds", eval("(W2 40)", lenv));
    assertEquals(10L, eval("(W1 40)", lenv));

    eval("(define a 999)", lenv);
    assertEquals(5L, eval("(begin (define a 10) (set! a 5) a)", lenv));
    assertEquals(20L, eval("(let ((a 5)) (set! a 10) (let () (set! a 15) (let () (+ a 5))))", lenv));

    eval("(define x 2)", lenv);
    eval("(define y 10)", lenv);
    eval("(define multiply (lambda (x y) (* x y)))", lenv);
    assertEquals(12L, eval("(+ x y)", lenv));
    assertEquals(100L, eval("(multiply y 10)", lenv));

    eval("(define x 10)", lenv);
    eval("(set! x 20)", lenv);
    assertEquals(20L, eval("x", lenv));
    eval("(define add (lambda (x y) (set! x (+ x y)) x))", lenv);
    assertEquals(110L, eval("(add 10 100)", lenv));

    eval("(define x 4)", lenv);
    eval("(define y 5)", lenv);
    assertEquals(2L, eval("(let ((x 1) (y 2)) (* x y))", lenv));
    assertEquals(20L, eval("(* x y)", lenv));
  }

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
  public void testEvalNumericalComparison() {
    assertEquals(TRUE,  eval("(= 1 1 1)", env));
    assertEquals(FALSE, eval("(= 1 0 1)", env));
    assertEquals(TRUE,  eval("(= 0)", env));
    assertEquals(TRUE,  eval("(= 0.57 0.5700)", env));
    assertEquals(TRUE,  eval("(= 7 7.00)", env));

    assertEquals(TRUE,  eval("(> 2 1)", env));
    assertEquals(TRUE,  eval("(> 2 1.123)", env));
    assertEquals(TRUE,  eval("(>= 2 1.123)", env));
    assertEquals(TRUE,  eval("(>= 2.5 1.123)", env));
    assertEquals(TRUE,  eval("(<= -2.5 1.123)", env));
    assertEquals(TRUE,  eval("(< -2.5 1.123)", env));
  }

  @Test
  public void testEvalNegation() {
    assertEquals(FALSE, eval("(not #t)",  env));
    assertEquals(TRUE,  eval("(not #f)",  env));
    assertEquals(TRUE,  eval("(not (= 1 2 1))", env));
    assertEquals(FALSE, eval("(not (= 1 1 1))", env));
  }

  // Equivalence
  @Test
  public void testEvalCharEq() {
    assertEquals(TRUE,  eval("(char=? #\\A #\\A)", env));
    assertEquals(FALSE, eval("(char=? #\\B #\\A)", env));
    assertEquals(TRUE,  eval("(char=? #\\newline #\\newline)", env));
  }

  @Test
  public void testEvalCharEqCi() {
    assertEquals(TRUE,  eval("(char-ci=? #\\Z #\\z)", env));
    assertEquals(FALSE, eval("(char-ci=? #\\b #\\A)", env));
  }

  @Test
  public void testEvalCharNumeric() {
    assertEquals(TRUE,  eval("(char-numeric? #\\1)", env));
    assertEquals(TRUE,  eval("(char-numeric? #\\0)", env));
    assertEquals(TRUE,  eval("(char-numeric? #\\9)", env));
    assertEquals(FALSE, eval("(char-numeric? #\\b)", env));
    assertEquals(FALSE, eval("(char-numeric? #\\.)", env));
    try {
      eval("(char-numeric? 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalCharWhitespace() {
    assertEquals(FALSE, eval("(char-whitespace? #\\1)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\0)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\9)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\b)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\.)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\backspace)", env));

    assertEquals(TRUE, eval("(char-whitespace? #\\newline)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\tab)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\vtab)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\return)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\space)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\page)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\linefeed)", env));
    try {
      eval("(char-whitespace? 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalCharAlphabetic() {
    assertEquals(FALSE, eval("(char-alphabetic? #\\1)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\0)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\9)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\.)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\backspace)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\newline)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\tab)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\vtab)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\return)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\space)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\page)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\linefeed)", env));

    assertEquals(TRUE, eval("(char-alphabetic? #\\b)", env));
    assertEquals(TRUE, eval("(char-alphabetic? #\\Z)", env));
    assertEquals(TRUE, eval("(char-alphabetic? #\\g)", env));
    assertEquals(TRUE, eval("(char-alphabetic? #\\I)", env));
    try {
      eval("(char-alphabetic? 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalCharUpperCase() {
    assertEquals(FALSE, eval("(char-upper-case? #\\1)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\0)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\9)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\b)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\.)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\a)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\z)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\i)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\h)", env));

    assertEquals(TRUE, eval("(char-upper-case? #\\A)", env));
    assertEquals(TRUE, eval("(char-upper-case? #\\Z)", env));
    assertEquals(TRUE, eval("(char-upper-case? #\\I)", env));
    assertEquals(TRUE, eval("(char-upper-case? #\\H)", env));
    try {
      eval("(char-upper-case? 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalCharLowerCase() {
    assertEquals(TRUE, eval("(char-lower-case? #\\b)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\a)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\z)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\i)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\h)", env));

    assertEquals(FALSE, eval("(char-lower-case? #\\A)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\Z)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\I)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\H)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\1)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\0)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\9)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\.)", env));
    try {
      eval("(char-lower-case? 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalCharToInteger() {
    assertEquals(49L, eval("(char->integer #\\1)", env));
    assertEquals(48L, eval("(char->integer #\\0)", env));
    assertEquals(57L, eval("(char->integer #\\9)", env));
    assertEquals(98L, eval("(char->integer #\\b)", env));
    assertEquals(46L, eval("(char->integer #\\.)", env));
    try {
      eval("(char->integer 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalIntegerToChar() {
    assertEquals('1', eval("(integer->char (char->integer #\\1))", env));
    assertEquals('0', eval("(integer->char (char->integer #\\0))", env));
    assertEquals('9', eval("(integer->char (char->integer #\\9))", env));
    assertEquals('b', eval("(integer->char (char->integer #\\b))", env));
    assertEquals('.', eval("(integer->char (char->integer #\\.))", env));
    try {
      eval("(integer->char #\\a)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: #\\a", e.getMessage());
    }
  }

  @Test
  public void testEvalCharUpcase() {
    assertEquals('1', eval("(char-upcase #\\1)", env));
    assertEquals('0', eval("(char-upcase #\\0)", env));
    assertEquals('9', eval("(char-upcase #\\9)", env));
    assertEquals('B', eval("(char-upcase #\\b)", env));
    assertEquals('Z', eval("(char-upcase #\\z)", env));
    assertEquals('.', eval("(char-upcase #\\.)", env));
    try {
      eval("(char-upcase 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalCharDowncase() {
    assertEquals('1', eval("(char-downcase #\\1)", env));
    assertEquals('0', eval("(char-downcase #\\0)", env));
    assertEquals('9', eval("(char-downcase #\\9)", env));
    assertEquals('b', eval("(char-downcase #\\B)", env));
    assertEquals('z', eval("(char-downcase #\\Z)", env));
    assertEquals('.', eval("(char-downcase #\\.)", env));
    try {
      eval("(char-downcase 1)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  // TODO Char comparison tests (#\z #\A #\H #\a #\X)

  @Test
  public void testEvalStringEq() {
    assertEquals(TRUE,  eval("(string=? \"test\" \"test\")", env));
    assertEquals(FALSE, eval("(string=? \"test\" \"test123\")", env));
    assertEquals(TRUE,  eval("(string=? \"\" \"\")", env));
    assertEquals(FALSE, eval("(string=? \"test\" \"Test\")", env));
  }

  @Test
  public void testEvalStringEqCi() {
    assertEquals(TRUE,  eval("(string-ci=? \"test\" \"test\")", env));
    assertEquals(FALSE, eval("(string-ci=? \"test\" \"test123\")", env));
    assertEquals(TRUE,  eval("(string-ci=? \"\" \"\")", env));
    assertEquals(TRUE,  eval("(string-ci=? \"test\" \"Test\")", env));
    assertEquals(TRUE,  eval("(string-ci=? \"tESt\" \"TesT\")", env));
  }

  @Test
  public void testEvalStringProc() {
    assertEquals("", eval("(string)", env));
    assertEquals("a", eval("(string #\\a)", env));
    assertEquals("abc", eval("(string #\\a #\\b #\\c)", env));

    try {
      eval("(string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalMakeString() {
    assertEquals("", eval("(make-string 0)", env));
    assertEquals("", eval("(make-string 0 #\\a)", env));
    assertEquals("a", eval("(make-string 1 #\\a)", env));
    assertEquals("aa", eval("(make-string 2 #\\a)", env));
    assertEquals("ZZZZZZZZ", eval("(make-string 8 #\\Z)", env));

    assertEquals("\u0000\u0000\u0000", eval("(make-string 3)", env));
    assertEquals("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000", eval("(make-string 8)", env));

    try {
      eval("(make-string \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: \"test\""));
    }
    try {
      eval("(make-string 2 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Character, actual: 1"));
    }
    try {
      eval("(make-string)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong number of arguments (0) passed to: make-string"));
    }
    try {
      eval("(make-string 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong number of arguments (3) passed to: make-string"));
    }
  }

  @Test
  public void testEvalStringLength() {
    assertEquals(0L, eval("(string-length \"\")", env));
    assertEquals(0L, eval("(string-length (string))", env));
    assertEquals(1L, eval("(string-length \"1\")", env));
    assertEquals(3L, eval("(string-length \"123\")", env));

    try {
      eval("(string-length 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: String, actual: 1"));
    }
  }

  @Test
  public void testEvalEq() {
    assertEquals(TRUE,  eval("(eq? '() '())", env));
    assertEquals(TRUE, eval("(eq? 1 1)", env));
    assertEquals(FALSE, eval("(eq? 1 2)", env));
    assertEquals(FALSE, eval("(eq? \"1\" \"1\")", env));
  }

  @Test
  public void testEvalEqv() {
    assertEquals(TRUE,  eval("(eqv? '() '())", env));
    assertEquals(TRUE,  eval("(eqv? 1 1)", env));
    assertEquals(FALSE, eval("(eqv? 1 2)", env));
    assertEquals(FALSE, eval("(eqv? \"1\" \"1\")", env));
  }

  @Test
  public void testEvalEqual() {
    assertEquals(TRUE,  eval("(equal? '() '())", env));
    assertEquals(TRUE,  eval("(equal? '(1 2 3) '( 1 2 3))", env));
    assertEquals(FALSE, eval("(equal? '(1 2 3 5) '( 1 2 3))", env));
    assertEquals(TRUE,  eval("(equal? 1 1)", env));
    assertEquals(FALSE, eval("(equal? 1 2)", env));
    assertEquals(TRUE,  eval("(equal? \"1fe\" \"1fe\")", env));
  }

  @Test
  public void testEvalDelayed() {

    assertEquals(1d, eval("(force (delay 1.0))", env));
    assertEquals("test", eval("(force (delay \"test\"))", env));
    assertEquals(10L, eval("(force (delay (+ 5 2 (* 1 3))))", env));
    assertEquals(SCMPromise.class, eval("(delay 1.0)", env).getClass());
    assertEquals(TRUE, eval("(promise? (delay 1.0))", env));
    assertEquals(3L, eval("(force (delay (+ 1 2)))", env));
    assertEquals(list(3L, 3L), eval("(let ((p (delay (+ 1 2))))(list (force p) (force p)))", env));
  }

  @Test
  public void testEvalIsChar() {
    assertEquals(TRUE, eval("(char? #\\A)", env));
    assertEquals(FALSE, eval("(char? \"A\")", env));
  }

  @Test
  public void testEvalIsString() {
    assertEquals(FALSE, eval("(string? #\\A)", env));
    assertEquals(TRUE, eval("(string? \"A\")", env));
  }

  @Test
  public void testEvalIsVector() {
    assertEquals(FALSE, eval("(vector? #\\A)", env));
    assertEquals(TRUE, eval("(vector? #(1 2 3 ))", env));
  }

  @Test
  public void testEvalIsList() {
    assertEquals(TRUE, eval("(list? '())", env));
    assertEquals(TRUE, eval("(list? '(1 2 3))", env));
    assertEquals(FALSE, eval("(list? #(1 2 3))", env));
    assertEquals(FALSE, eval("(list? (cons 1 2))", env));
    assertEquals(FALSE, eval("(list? 2)", env));
    assertEquals(TRUE, eval("(list? (car '((1 2 3))))", env));
    assertEquals(TRUE, eval("(list? (cdr '((1 2 3))))", env));
    // TODO
    assertEquals(FALSE, eval("(list? (car '((1 . 2))))", env));
    assertEquals(FALSE, eval("(list? (vector-ref #((1 2 3 . 4)) 0))", env));
    assertEquals(FALSE, eval("(list? (vector-ref #((1 . 2)) 0))", env));
  }

  @Test
  public void testEvalVector() {
    assertEquals(new SCMVector(), eval("#()", env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval("#(1 2 3 )", env));

    assertEquals(new SCMVector(), eval("(vector)", env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval("(vector 1 2 3)", env));
    assertEquals(new SCMVector(1L, 2L, 3L), eval("(vector 1 2 (+ 1 2))", env));
  }

  @Test
  public void testEvalMakeVector() {
    assertEquals(new SCMVector(1L, 1L, 1L), eval("(make-vector 3 1)", env));
    assertEquals(new SCMVector(), eval("(make-vector 0)", env));
    assertEquals(new SCMVector(UNSPECIFIED, UNSPECIFIED, UNSPECIFIED), eval("(make-vector 3)", env));
    try {
      eval("(make-vector 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong number of arguments (3) passed to: make-vector"));
    }

    try {
      eval("(make-vector \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: \"test\""));
    }
  }

  @Test
  public void testEvalVectorLength() {
    assertEquals(0L, eval("(vector-length #())", env));
    assertEquals(0L, eval("(vector-length (vector))", env));
    assertEquals(3L, eval("(vector-length (vector 1 2 3))", env));

    try {
      eval("(vector-length 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: 1"));
    }
  }

  @Test
  public void testEvalVectorRef() {
    assertEquals(1L, eval("(vector-ref (vector 1 2 3) 0)", env));
    assertEquals(2L, eval("(vector-ref (vector 1 2 3) 1)", env));
    assertEquals(3L, eval("(vector-ref (vector 1 2 3) 2)", env));
    assertEquals("test", eval("(vector-ref (vector \"test\" 2 3) 0)", env));

    try {
      eval("(vector-ref (vector 1 2 3) -1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }
    try {
      eval("(vector-ref (vector 1 2 3) 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }
    try {
      eval("(vector-ref (vector) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }
    try {
      eval("(vector-ref '(1 2 3) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: (1 2 3)"));
    }
    try {
      eval("(vector-ref (vector 1 2 3) 0.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
  }

  @Test
  public void testEvalVectorSet() {

    String sexp = "(begin (define v (vector 1 2 3))" +
                  "       (vector-set! v 0 99)" +
                  "       (vector-ref  v 0))";
    assertEquals(99L, eval(sexp, env));

    sexp = "(begin (define v (vector 1 2 3))" +
           "       (vector-set! v 2 \"test\")" +
           "       (vector-ref  v 2))";
    assertEquals("test", eval(sexp, env));

    sexp = "(begin (define v (vector 1 2 3))" +
           "       (vector-set! v -1 \"test\"))";
    try {
      eval(sexp, env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }

    sexp = "(begin (define v (vector 1 2 3))" +
           "       (vector-set! v 3 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }

    sexp = "(begin (define v (vector))" +
           "       (vector-set! v 0 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }

    sexp = "(begin (define v '(1 2 3))" +
           "       (vector-set! v 0 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Vector, actual: (1 2 3)"));
    }

    sexp = "(begin (define v (vector 1 2))" +
           "       (vector-set! v 0.5 \"test\"))";
    try {
      eval(sexp, env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
  }

  @Test
  public void testEvalListToVector() {

    assertEquals(new SCMVector(1L, 2L, "test"), eval("(list->vector '(1 2 \"test\"))", env));
    assertEquals(new SCMVector(), eval("(list->vector '())", env));

    try {
      eval("(list->vector #(1 2 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: #(1 2 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalVectorToList() {

    assertEquals(list(1L, 2L, "test"), eval("(vector->list #(1 2 \"test\"))", env));
    assertEquals(list(), eval("(vector->list #())", env));

    try {
      eval("(vector->list '(1 2 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Vector, actual: (1 2 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalVectorFill() {

    String sexp = "(begin (define v (vector 1 2 3))" +
                  "       (vector-fill! v 3)" +
                  "       v)";
    assertEquals(new SCMVector(3L, 3L, 3L), eval(sexp, env));

    sexp = "(begin (define v (vector))" +
           "       (vector-fill! v 3)" +
           "       v)";
    assertEquals(new SCMVector(), eval(sexp, env));

    sexp = "(begin (define v (list 1 2 3))" +
           "       (vector-fill! v 3)" +
           "       v)";
    try {
      eval(sexp, env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Vector, actual: (1 2 3)", e.getMessage());
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

    // internal define
    assertEquals(45L, eval("(let ((x 5))(define foo (lambda (y) (bar x y)))(define bar (lambda (a b) (+ (* a b) a)))(foo (+ x 3)))", env));
    try {
      eval("(foo 5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Unbound variable: foo"));
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
  public void testEvalQuine() {

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintStream old = System.out;
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));

    String quine = "((lambda (s) (display (list s (list (quote quote) s))))" +
                   " (quote (lambda (s) (display (list s (list (quote quote) s))))))";
    eval(quine, tempEnv);
    assertEquals(quine, baos.toString().trim());

    System.setOut(old);
  }

  @Test
  public void testEvalLambda() {
    String f1 = "(lambda ())";
    try {
      eval(f1, env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().contains("bad lambda in form: " + f1));
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
  }

  @Test
  public void testEvalWhen() {
    assertEquals(0L, eval("(when #t 5 4 3 2 1 0)", env));
    assertEquals(UNSPECIFIED, eval("(when #f 5 4 3 2 1 0)", env));
  }

  @Test
  public void testEvalQuote() {
    assertEquals(0L, eval("'0", env));
    assertEquals("test", eval("'\"test\"", env));
    assertEquals(SCMCons.<Object>list(new SCMSymbol(SCMSpecialForm.QUOTE.toString()), "test"), eval("''\"test\"", env));
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
    assertEquals("DONE", eval(doTest3, env));

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
        "(is-odd?  (lambda (n) (and (not (= n 0)) (is-even? (- n 1))))))" +
        "  (is-odd? 11))";
    assertEquals(TRUE, eval(letrec1, env));
  }

  @Test
  public void testEvalCond() {
    // "Source expression failed to match any pattern in form (cond)"
    try {
      eval("(cond)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (cond)"));
    }
    // "Invalid clause in subform "
    try {
      eval("(cond 1)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().equals("Invalid clause in subform 1"));
    }
    // "cond: else must be the last clause in subform"
    try {
      eval("(cond (else 1) (#t 5))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().equals("cond: else must be the last clause in subform"));
    }
    // "Source expression failed to match any pattern in form (cond)"
    try {
      eval("(cond)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (cond)"));
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
      assertTrue(e.getMessage().equals("Source expression failed to match any pattern in form (case)"));
    }
    try {
      eval("(case 1 1)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().equals("Invalid clause in subform 1"));
    }
    try {
      eval("(case (* 2 3) (else 'prime) ((1 4 6 8 9) 'composite))", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertTrue(e.getMessage().equals("case: else must be the last clause in subform"));
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
    PrintStream old = System.out;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    System.setOut(new PrintStream(baos));
    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));
    assertEquals(UNSPECIFIED, eval("(begin (display \"4 plus 1 equals \")(display (+ 4 1)))", tempEnv));
    System.setOut(old);
  }

  @Test
  public void testEvalClassOf() {
    // class-of
    assertEquals(Long.class.getName(), eval("(class-of 1)", env));
    assertEquals(Double.class.getName(), eval("(class-of 1.0)", env));
    assertEquals(SCMPromise.class.getName(), eval("(class-of (delay 1))", env));
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
  public void testEvalList() {
    assertEquals(SCMCons.class.getName(), eval("(class-of (list 1 2 3 4 5))", env));
    assertEquals(list(1L, 2L, 3L), eval("(list 1 2 3)", env));
  }

  @Test
  public void testEvalEmpty() {
    assertEquals(TRUE,  eval("(null?  '())", env));
    assertEquals(TRUE,  eval("(empty? '())", env));
    assertEquals(FALSE, eval("(null?  '(1 2 3))", env));
    assertEquals(FALSE, eval("(empty? '(1 2 3))", env));
    assertEquals(FALSE, eval("(null?  1)", env));
    assertEquals(FALSE, eval("(empty? 1)", env));
    assertEquals(TRUE,  eval("(null? (cdr '(1)))", env));
  }

  @Test
  public void testEvalLength() {
    assertEquals(0L, eval("(length '())", env));
    assertEquals(1L, eval("(length '(1))", env));
    assertEquals(5L, eval("(length '(1 2 3 4 5))", env));
    try {
      eval("(length)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 1) passed to: length", e.getMessage());
    }
    try {
      eval("(length 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalMember() {
    assertEquals(FALSE, eval("(member 0 '())", env));
    assertEquals(FALSE, eval("(member 0 '(1 2 3))", env));
    assertEquals(FALSE, eval("(member \"test\" '(1 2 3))", env));

    assertEquals(list(1L, 2L, 3L), eval("(member 1 '(1 2 3))", env));
    assertEquals(list(2L, 3L), eval("(member 2 '(1 2 3))", env));
    assertEquals(list(3L), eval("(member 3 '(1 2 3))", env));
    assertEquals(list(list(new SCMSymbol("a")), new SCMSymbol("c")), eval("(member (list 'a) '(b (a) c))", env));
    try {
      eval("(member)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 2) passed to: member", e.getMessage());
    }
    try {
      eval("(member 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument to `member`! Expected: List, Actual: #()", e.getMessage());
    }
  }

  @Test
  public void testEvalMemq() {
    assertEquals(FALSE, eval("(memq 0 '())", env));
    assertEquals(FALSE, eval("(memq 0 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memq \"test\" '(1 2 3))", env));

    assertEquals(list(1L, 2L, 3L), eval("(memq 1 '(1 2 3))", env));
    assertEquals(list(2L, 3L), eval("(memq 2 '(1 2 3))", env));
    assertEquals(list(3L), eval("(memq 3 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memq (list 'a) '(b (a) c))", env));

    assertEquals(list(new SCMSymbol("a"), new SCMSymbol("b"), new SCMSymbol("c")), eval("(memq 'a '(a b c))", env));
    assertEquals(list(new SCMSymbol("b"), new SCMSymbol("c")), eval("(memq 'b '(a b c))", env));
    assertEquals(FALSE, eval("(memq 'a '(b c d))", env));
    try {
      eval("(memq)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 2) passed to: memq", e.getMessage());
    }
    try {
      eval("(memq 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument to `memq`! Expected: List, Actual: #()", e.getMessage());
    }
  }

  @Test
  public void testEvalMemv() {
    assertEquals(FALSE, eval("(memv 0 '())", env));
    assertEquals(FALSE, eval("(memv 0 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memv \"test\" '(1 2 3))", env));

    assertEquals(list(1L, 2L, 3L), eval("(memv 1 '(1 2 3))", env));
    assertEquals(list(2L, 3L), eval("(memv 2 '(1 2 3))", env));
    assertEquals(list(3L), eval("(memv 3 '(1 2 3))", env));
    assertEquals(FALSE, eval("(memv (list 'a) '(b (a) c))", env));

    assertEquals(list(new SCMSymbol("a"), new SCMSymbol("b"), new SCMSymbol("c")), eval("(memv 'a '(a b c))", env));
    assertEquals(list(new SCMSymbol("b"), new SCMSymbol("c")), eval("(memv 'b '(a b c))", env));
    assertEquals(FALSE, eval("(memv 'a '(b c d))", env));

    assertEquals(list(101L, 102L), eval("(memv 101 '(100 101 102))", env));
    try {
      eval("(memv)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 2) passed to: memv", e.getMessage());
    }
    try {
      eval("(memv 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument to `memv`! Expected: List, Actual: #()", e.getMessage());
    }
  }

  @Test
  public void testEvalAssoc() {
    eval("(define e '((a 1) (b 2) (c 3)))", env);
    assertEquals(list((Object)list(new SCMSymbol("a"))), eval("(assoc (list 'a) '(((a)) ((b)) ((c))))", env));
    try {
      eval("(assoc)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 2) passed to: assoc", e.getMessage());
    }
    try {
      eval("(assoc 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument to `assoc`! Expected: List, Actual: #()", e.getMessage());
    }
    try {
      eval("(assoc 1 '((a 2) 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalAssq() {
    eval("(define e '((a 1) (b 2) (c 3)))", env);
    assertEquals(list(new SCMSymbol("a"), 1L), eval("(assq 'a e)", env));
    assertEquals(list(new SCMSymbol("b"), 2L), eval("(assq 'b e)", env));
    assertEquals(FALSE, eval("(assq 'd e)", env));
    try {
      eval("(assq)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 2) passed to: assq", e.getMessage());
    }
    try {
      eval("(assq 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument to `assq`! Expected: List, Actual: #()", e.getMessage());
    }
    try {
      eval("(assq 1 '((a 2) 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalAssv() {
    assertEquals(list(5L, 7L), eval("(assv 5 '((2 3) (5 7) (11 13)))", env));
    try {
      eval("(assv)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: 2) passed to: assv", e.getMessage());
    }
    try {
      eval("(assv 1 #())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument to `assv`! Expected: List, Actual: #()", e.getMessage());
    }
    try {
      eval("(assv 1 '((a 2) 3))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong type argument in position 1 (expecting association list): ((a 2) 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalIsZero() {
    assertEquals(TRUE,  eval("(zero? 0)", env));
    assertEquals(TRUE,  eval("(zero? 0.0)", env));
    assertEquals(FALSE, eval("(zero? 1)", env));
    assertEquals(FALSE, eval("(zero? -5)", env));

    try {
      eval("(zero? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalNegative() {
    assertEquals(FALSE, eval("(negative? 0)", env));
    assertEquals(FALSE, eval("(negative? 0.0)", env));
    assertEquals(FALSE, eval("(negative? 1)", env));
    assertEquals(FALSE, eval("(negative? (* -5 -6))", env));
    assertEquals(TRUE,  eval("(negative? -5)", env));
    try {
      eval("(negative? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalPositive() {
    assertEquals(FALSE, eval("(positive? 0)", env));
    assertEquals(FALSE, eval("(positive? 0.0)", env));
    assertEquals(TRUE,  eval("(positive? 1)", env));
    assertEquals(TRUE,  eval("(positive? (* -5 -6))", env));
    assertEquals(FALSE, eval("(positive? -5)", env));
    try {
      eval("(positive? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalEven() {
    assertEquals(TRUE,  eval("(even? 0)", env));
    assertEquals(TRUE,  eval("(even? 0.0)", env));
    assertEquals(TRUE,  eval("(even? 4)", env));
    assertEquals(TRUE,  eval("(even? 100)", env));
    assertEquals(FALSE, eval("(even? 1)", env));
    assertEquals(TRUE,  eval("(even? (* -5 -6))", env));
    assertEquals(FALSE, eval("(even? -5)", env));
    try {
      eval("(even? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalOdd() {
    assertEquals(FALSE, eval("(odd? 0)", env));
    assertEquals(FALSE, eval("(odd? 0.0)", env));
    assertEquals(FALSE, eval("(odd? 4)", env));
    assertEquals(FALSE, eval("(odd? 100)", env));
    assertEquals(TRUE,  eval("(odd? 1)", env));
    assertEquals(FALSE, eval("(odd? 4)", env));
    assertEquals(FALSE, eval("(odd? (* -5 -6))", env));
    assertEquals(TRUE,  eval("(odd? -5)", env));
    try {
      eval("(odd? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalRound() {
    assertEquals(0L,   eval("(round 0)",    env));
    assertEquals(4L,   eval("(round 4)",    env));
    assertEquals(-4L,  eval("(round -4)",   env));
    assertEquals(0.0,  eval("(round 0.0)",  env));
    assertEquals(1.0,  eval("(round 1.0)",  env));
    assertEquals(2.0,  eval("(round 1.5)",  env));
    assertEquals(-2.0, eval("(round -1.5)", env));
    assertEquals(2.0,  eval("(round 2.5)",  env));
    assertEquals(-0.0, eval("(round -0.5)", env));
    assertEquals(-2.0, eval("(round -1.7)", env));
    assertEquals(4.0,  eval("(round 3.7)",  env));
    assertEquals(3.0,  eval("(round 2.7)",  env));
    assertEquals(2.0,  eval("(round 2.5)",  env));
    try {
      eval("(round \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalFloor() {
    assertEquals(0L,   eval("(floor 0)",    env));
    assertEquals(4L,   eval("(floor 4)",    env));
    assertEquals(-5.0, eval("(floor -4.3)", env));
    assertEquals(3.0,  eval("(floor 3.5)",  env));
    assertEquals(1.0,  eval("(floor 1.2)",  env));
    assertEquals(-2.0, eval("(floor -1.2)", env));
    try {
      eval("(floor \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalCeiling() {
    assertEquals(0L,   eval("(ceiling 0)",    env));
    assertEquals(4L,   eval("(ceiling 4)",    env));
    assertEquals(-4.0, eval("(ceiling -4.3)", env));
    assertEquals(4.0,  eval("(ceiling 3.5)",  env));
    assertEquals(2.0,  eval("(ceiling 1.2)",  env));
    assertEquals(-1.0, eval("(ceiling -1.2)", env));
    try {
      eval("(ceiling \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalTruncate() {
    assertEquals(0L,   eval("(truncate 0)",    env));
    assertEquals(4L,   eval("(truncate 4)",    env));
    assertEquals(-4L,  eval("(truncate -4)",   env));
    assertEquals(3.0,  eval("(truncate 3.5)",  env));
    assertEquals(-3.0, eval("(truncate -3.5)", env));
    assertEquals(2.0,  eval("(truncate 2.2)",  env));
    assertEquals(-1.0, eval("(truncate -1.2)", env));
    try {
      eval("(truncate \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalMax() {
    assertEquals(0L,   eval("(max 0)",    env));
    assertEquals(5.0,  eval("(max 5.0)",  env));
    assertEquals(-5.0, eval("(max -5.0)", env));
    assertEquals(-5.0, eval("(max -6 -7 -5.0)", env));
    assertEquals(7.0,  eval("(max 6 7 5.0)",    env));

    try {
      eval("(max \"test\" 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }

    try {
      eval("(max 0 \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalMin() {
    assertEquals(0L,   eval("(min 0)",    env));
    assertEquals(5.0,  eval("(min 5.0)",  env));
    assertEquals(-5.0, eval("(min -5.0)", env));
    assertEquals(-7.0, eval("(min -6 -7 -5.0)", env));
    assertEquals(5.0,  eval("(min 6 7 5.0)",    env));
    try {
      eval("(min \"test\" 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }

    try {
      eval("(min 0 \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testEvalGCD() {
    // gcd of no args is 0
    assertEquals(0L, eval("(gcd)", env));
    // gcd of 0(s) is 0
    assertEquals(0L, eval("(gcd 0)", env));
    assertEquals(0d, eval("(gcd 0.0)", env));
    assertEquals(0L, eval("(gcd 0 0)", env));
    assertEquals(0d, eval("(gcd 0 0.0)", env));
    assertEquals(5L, eval("(gcd 5 0)", env));
    assertEquals(5d, eval("(gcd 5.0 0)", env));
    assertEquals(5L, eval("(gcd 0 5)", env));

    // gcd of n is n
    assertEquals(5L, eval("(gcd 5)", env));
    assertEquals(5L, eval("(gcd -5)", env));

    // gcd of multiple numbers
    assertEquals(5L, eval("(gcd 5 10)", env));
    assertEquals(1L, eval("(gcd 3 6 8)", env));

    // TODO Doubles
    assertEquals(3d, eval("(gcd 3.0 6)", env));
    assertEquals(40000d, eval("(gcd 200000.0 40000.0)", env));
    try {
      eval("(gcd 3.3 6)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: 3.3", e.getMessage());
    }

    assertEquals(new BigDecimal("9"), eval("(gcd 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 9)", env));

    /* Check switch from Double to BigDecimal for big numbers */
    assertEquals(3L,                  eval("(gcd 99999999999999999 123)", env));
    assertEquals(3L,                  eval("(gcd 999999999999999999 123)", env));
    assertEquals(new BigDecimal("3"),   eval("(gcd 9999999999999999999 123)", env));
    assertEquals(new BigDecimal("123"), eval("(gcd 99999999999999999999 123)", env));
    assertEquals(new BigDecimal("3"),   eval("(gcd 999999999999999999999 123)", env));
    assertEquals(new BigDecimal("3"),   eval("(gcd 9999999999999999999999 123)", env));
  }

  @Test
  public void testEvalLCM() {
    // lcm of no args is 0
    assertEquals(0L, eval("(lcm)", env));
    // lcm of 0(s) is 0
    assertEquals(0L, eval("(lcm 0)", env));
    assertEquals(0d, eval("(lcm 0.0)", env));
    assertEquals(0L, eval("(lcm 0 0)", env));

    // lcm of n is n
    assertEquals(5L, eval("(lcm 5)", env));
    assertEquals(5d, eval("(lcm 5.0)", env));
    assertEquals(5L, eval("(lcm -5)", env));

    // lcm of multiple numbers
    assertEquals(10L, eval("(lcm 5 10)", env));
    assertEquals(24L, eval("(lcm 3 6 8)", env));
    assertEquals(24d, eval("(lcm 3 6 8.0)", env));

    // TODO Doubles
    assertEquals(6d, eval("(lcm 3.0 6)", env));
    assertEquals(200000d, eval("(lcm 200000.0 40000.0)", env));
    try {
      eval("(lcm 3.3 6)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: 3.3", e.getMessage());
    }

    String big = "99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";
    assertEquals(new BigDecimal(big), eval(String.format("(lcm %s 9)", big), env));
  }

  @Test
  public void testEvalExpt() {
    assertEquals(1.0, eval("(expt 9 0)", env));
    assertEquals(0.0, eval("(expt 0 10)", env));
    assertEquals(1.0, eval("(expt 1 1)", env));
    assertEquals(8.0, eval("(expt 2 3)", env));
    assertEquals(16777216.0, eval("(expt 4 12)", env));
    assertEquals(25.0, eval("(expt -5 2)", env));
    assertEquals(-125.0, eval("(expt -5 3)", env));
    assertEquals(13.489468760533386, eval("(expt 2.2 3.3)", env));
    try {
      eval("(expt \"test\" 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
    try {
      eval("(expt 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong number of arguments (actual: 1, expected: 2) passed to: expt", e.getMessage());
    }
  }

  @Test
  public void testBigDecimal() {

    String big0 = "2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
    assertEquals(new BigDecimal(big0), eval(big0, env));

    BigDecimal big1 = new BigDecimal("46253220748483971668312221557921994865355615937726643119142534763573384386717010179" +
        "33035570379376866259256597659610145236756255853523897837700465755231009606767424706" +
        "73348011864143704094117912896787287887428473700404535710995567015427340013704721113" +
        "33316343260021388334157118647272603383652782933091786683436259682248241271823609303" +
        "74088996645418723950501545025386234033486857262740020740808886229945286599837304752" +
        "25837944037056491016035642078654527374207462210630264065442615967117912099739418879" +
        "84132513");

    assertEquals(big1, eval("(expt 33 333)", env));
    assertEquals(TRUE, eval(String.format("(number? %s)", big0), env));

    assertEquals(new BigDecimal(big0), eval(String.format("(* (/ %s 10) 10)", big0), env));
    assertEquals(new BigDecimal(big0).multiply(new BigDecimal("2")), eval(String.format("(+ %s %s)", big0, big0), env));
    assertEquals(new BigDecimal(big0).multiply(new BigDecimal("2")).subtract(new BigDecimal(big0)),
                 eval(String.format("(- (* 2 %s) %s)", big0, big0), env));


    assertEquals(new BigDecimal(big0), eval(String.format("(truncate (+ 0.2 %s))", big0),  env));
    assertEquals(new BigDecimal(big0).negate(), eval(String.format("(truncate (+ 0.2 -%s))", big0),  env));
    assertEquals(new BigDecimal(big0), eval(String.format("(floor (+ 0.2 %s))", big0),  env));
    assertEquals(new BigDecimal(big0).add(BigDecimal.ONE),
                 eval(String.format("(ceiling (+ 0.2 %s))", big0),  env));

    assertEquals(new BigDecimal(big0), eval(String.format("(abs -%s)", big0),  env));
    assertEquals(new BigDecimal(big0).add(BigDecimal.ONE),
                 eval(String.format("(max (+ 1 %s) %s)", big0, big0),  env));

    assertEquals(new BigDecimal(big0),
                 eval(String.format("(min (+ 1 %s) %s)", big0, big0),  env));

    String big2 = "941737268473075634481294063531333847658485002458168527101639838005582185517473483816983389228732066437165294377295109210176795859047876399460771530181828861843994801526320659067260600443063376955200810073997787724454002350759571876705644517946943898492214066331998886559185229835330687165577365519449395424366904222913306696961330084086377946063169138303897697242206192836209273444873251023411764271944704088313845446589768727760791185170266144604537045173629663045739300767985189493967771010336173962367396474652866334212802605674879313278209206179544726008444885447395757991883875945457869103573901612777316112247438629624081718143710269108788904389008167209091151002216893051746019091645742839251513268837094248809018521046734530253606053753445604156050903737280600427015788467630468023527367174845920094011539693975275654700093627716413";
    assertEquals(BigDecimal.ONE, eval(String.format("(modulo %s 4)", big2), env));
    assertEquals(new BigDecimal("-2"), eval(String.format("(modulo %s -5)", big2), env));

    assertEquals(BigDecimal.ONE, eval(String.format("(remainder %s 4)", big2), env));
    assertEquals(new BigDecimal("3"), eval(String.format("(remainder %s -5)", big2), env));

    String quotientResult1 = "470868634236537817240647031765666923829242501229084263550819919002791092758736741908491694614366033218582647188647554605088397929523938199730385765090914430921997400763160329533630300221531688477600405036998893862227001175379785938352822258973471949246107033165999443279592614917665343582788682759724697712183452111456653348480665042043188973031584569151948848621103096418104636722436625511705882135972352044156922723294884363880395592585133072302268522586814831522869650383992594746983885505168086981183698237326433167106401302837439656639104603089772363004222442723697878995941937972728934551786950806388658056123719314812040859071855134554394452194504083604545575501108446525873009545822871419625756634418547124404509260523367265126803026876722802078025451868640300213507894233815234011763683587422960047005769846987637827350046813858206";
    assertEquals(new BigDecimal(quotientResult1), eval(String.format("(quotient %s 2)", big2), env));
    assertEquals(new BigDecimal(2), eval(String.format("(quotient %s (quotient %s 2))", big2, big2), env));

    assertEquals(TRUE, eval(String.format("(eqv? %s %s)", big2, big2), env));
    assertEquals(TRUE, eval(String.format("(<= %s %s)", big2, big2), env));
    assertEquals(FALSE, eval(String.format("(< %s %s)", big2, big2), env));
    assertEquals(TRUE, eval(String.format("(> (+ 1 %s) %s)", big2, big2), env));
    assertEquals(TRUE, eval(String.format("(< (+ 1 2) %s)", big2), env));

    // FIXME
    assertEquals(Double.POSITIVE_INFINITY, eval(String.format("(sqrt %s)", big2), env));
  }

  @Test
  public void testEvalIsInteger() {

    assertEquals(TRUE,  eval("(integer? 0)", env));
    assertEquals(TRUE,  eval("(integer? 0.0)", env));
    assertEquals(TRUE,  eval("(integer? 4)", env));
    assertEquals(TRUE,  eval("(integer? 100)", env));
    assertEquals(TRUE,  eval("(integer? 1)", env));
    assertEquals(TRUE,  eval("(integer? (* -5 -6))", env));
    assertEquals(TRUE,  eval("(integer? -5)", env));
    assertEquals(FALSE, eval("(integer? -5.4)", env));
    assertEquals(FALSE, eval("(integer? 3.14)", env));
    assertEquals(FALSE, eval("(integer? .123)", env));
    try {
      eval("(integer? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Number, actual: \"test\"", e.getMessage());
    }
  }

  @Test
  public void testIntegerRoots() {

    String integerRoots = "(define (root a b)" +
                          "  (define // quotient)" +
                          "  (define (y a a1 b c d e)" +
                          "    (if (or (= c d) (= c e))" +
                          "        (min d e)" +
                          "      (y a a1 b d e (// (+ (* a1 e)" + // <--- y is recursive. TCO
                          "                           (// b (expt e a1))) a))))" +
                          "  (if (< b 2)" +
                          "      b" +
                          "    (let* ((a1 (- a 1))" +
                          "           (c 1)" +
                          "           (d (// (+ (* a1 c) (// b (expt c a1))) a))" +
                          "           (e (// (+ (* a1 d) (// b (expt d a1))) a)))" +
                          "      (y a a1 b c d e))))";

    IEnvironment tempEnv = new DefaultEnvironment();
    eval(integerRoots, tempEnv);
    // TODO StackOverflow
//    assertEquals(2.0, eval("(root 3 (* 2 (expt 1000 2000)))", tempEnv));
  }

  // TODO
  @Test
  public void testTCO() {
    String recursive = "(define (recursive n)" +
                       "  (if (zero? n)" +
                       "      \"DONE\"" +
                       "    (recursive (- n 1))))";
    eval(recursive, env);

//    assertEquals("DONE", eval("(recursive 5)", env));
//    assertEquals("DONE", eval("(recursive 489)", env));
//    assertEquals("DONE", eval("(recursive 490)", env));
//    assertEquals("DONE", eval("(recursive 1000000)", env));
  }

  @Test
  public void testEvalHanoi() {

    PrintStream old = System.out;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));
    tempEnv.put(new SCMSymbol("newline"), new Newline(System.out));

    String hanoi = "(define (hanoi n a b c) (if (> n 0) (begin (hanoi (- n 1) a c b) (display \"Move disk from pole \") (display a) (display \" to pole \") (display b) (newline) (hanoi (- n 1) c b a)) #t))";
    eval(hanoi, tempEnv);
    eval("(hanoi 4 1 2 3)", tempEnv);

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
  public void testDayOfWeek() {

    String dayOfWeek = "(define (day-of-week year month day)" +
                       "  (if (< month 3)" +
                       "    (begin" +
                       "      (set! month (+ month 12))" +
                       "      (set! year (- year 1)))" +
                       "  #f)" +
                       "(+ 1 " +
                       "   (remainder (+ 5 day (quotient (* (+ 1 month) 13) 5) " +
                       "                 year (quotient year 4) (* (quotient year 100) 6) (quotient year 400)) " +
                       "              7)))";
    eval(dayOfWeek, env);
    assertEquals(3L, eval("(day-of-week 2016 6 29)", env));

    // test Today
    Calendar calendar = Calendar.getInstance();
    Integer dw = calendar.get(Calendar.DAY_OF_WEEK) - 1;
    if (dw == 0) {
      dw = 7;
    }
    int y = calendar.get(Calendar.YEAR);
    int m = calendar.get(Calendar.MONTH) + 1;
    int d = calendar.get(Calendar.DAY_OF_MONTH);
    String todaySexp = "(day-of-week %s %s %s)";
    assertEquals(dw.longValue(), eval(String.format(todaySexp, y, m, d), env));
  }

  @Test
  public void testEvalSymbolStringConversion() {

    assertEquals("test", eval("(symbol->string 'test)", env));
    assertEquals("test", eval("(symbol->string (string->symbol (symbol->string 'test)))", env));
    assertEquals(new SCMSymbol("test"), eval("(string->symbol (symbol->string 'test))", env));

    try {
      eval("(symbol->string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: Symbol, actual: 1"));
    }
    try {
      eval("(string->symbol 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: String, actual: 1"));
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
  public void testCons() {

    assertEquals(TRUE, eval("(equal? '(1 2)   (cons 1 (cons 2 '())))", env));
    assertEquals(TRUE, eval("(equal? '(1 2 3) (cons 1 (cons 2 (cons 3 '()))))", env));
    assertEquals(TRUE, eval("(equal? '(1 2 3) (cons 1 '(2 3))))", env));
    assertEquals(TRUE, eval("(equal? '(1)     (cons 1 '())))", env));
    assertEquals(TRUE, eval("(equal? (cons 1 2) (cons 1 2)))", env));

    // check that we do not modify the original list/cons, but return new instead
    eval("(define conslist '())", env);
    eval("(cons 1 conslist)", env);
    assertEquals(TRUE, eval("(equal? '() conslist))", env));
    eval("(define conslist '(3))", env);
    eval("(cons 1 conslist)", env);
    assertEquals(TRUE, eval("(equal? '(3) conslist))", env));
  }

  @Test
  public void testIsPair() {

    assertEquals(FALSE, eval("(pair? '())", env));
    assertEquals(FALSE, eval("(pair? 1)", env));
    assertEquals(FALSE, eval("(pair? #(1 2))", env));
    assertEquals(TRUE,  eval("(pair? '(1))", env));
    assertEquals(TRUE,  eval("(pair? '(1 2))", env));
    assertEquals(TRUE,  eval("(pair? '(1 2 3))", env));
    assertEquals(TRUE,  eval("(pair? (cons 1 2))", env));
    assertEquals(TRUE,  eval("(pair? (cons 1 '()))", env));
    assertEquals(TRUE,  eval("(pair? (cons 1 (cons 2 3))))", env));
  }

  @Test
  public void testCar() {

    assertEquals(1L, eval("(car (cons 1 2))", env));
    assertEquals("test", eval("(car (cons \"test\" 2))", env));
    assertEquals(1L, eval("(car (cons 1 (cons 2 3)))", env));
    assertEquals(1L, eval("(car '(1 2 3))", env));
    assertEquals(1L, eval("(car '(1))", env));
    assertEquals(1L, eval("(car (list 1))", env));
    try {
      eval("(car '())", env);
    } catch (IllegalArgumentException e) {
     assertEquals("Wrong argument type. Expected: Pair, actual: ()", e.getMessage());
    }
    try {
      eval("(car 1)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testCdr() {

    assertEquals(2L, eval("(cdr (cons 1 2))", env));
    assertEquals("test", eval("(cdr (cons 2 \"test\"))", env));
    assertEquals(cons(2L, 3L), eval("(cdr (cons 1 (cons 2 3)))", env));
    assertEquals(list(2L, 3L), eval("(cdr '(1 2 3))", env));
    assertEquals(SCMCons.NIL, eval("(cdr '(1))", env));
    assertEquals(SCMCons.NIL, eval("(cdr (list 1))", env));
    try {
      eval("(cdr '())", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: ()", e.getMessage());
    }
    try {
      eval("(cdr 1)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testSetCar() {

    assertEquals(UNSPECIFIED, eval("(set-car! '(1) 2)", env));
    assertEquals(3L, eval("(let ((a '(1))) (set-car! a 3) (car a)))", env));
    assertEquals("test", eval("(let ((a '(1 2 3))) (set-car! a \"test\") (car a)))", env));
    assertEquals("test", eval("(let ((a (cons 3 4))) (set-car! a \"test\") (car a)))", env));
    try {
      eval("(set-car! '() 1)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: ()", e.getMessage());
    }
    try {
      eval("(set-car! 5 1)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: 5", e.getMessage());
    }
  }

  @Test
  public void testSetCdr() {

    assertEquals(UNSPECIFIED, eval("(set-cdr! '(1) 2)", env));
    assertEquals(3L, eval("(let ((a '(1))) (set-cdr! a 3) (cdr a)))", env));
    assertEquals("test", eval("(let ((a '(1))) (set-cdr! a \"test\") (cdr a)))", env));
    assertEquals(list(2L, 3L, 4L), eval("(let ((a '(1))) (set-cdr! a '(2 3 4)) (cdr a)))", env));
    assertEquals(3L, eval("(let ((a (cons 1 2))) (set-cdr! a 3) (cdr a)))", env));
    assertEquals(2L, eval("(let ((a (cons 1 2))) (set-cdr! a '(3 4 5)) (cdr (cons 1 2)))", env));
    try {
      eval("(set-cdr! '() 1)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: ()", e.getMessage());
    }
    try {
      eval("(set-cdr! 5 1)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Pair, actual: 5", e.getMessage());
    }
  }

  @Test
  public void testAppend() {
    assertEquals("test", eval("(append '() \"test\")", env));
    assertEquals(5L, eval("(append '() 5)", env));
    assertEquals(cons(1L, 5L), eval("(append '(1) 5)", env));
    assertEquals(list(1L, 2L, 3L), eval("(append '(1) '(2 3))", env));
    assertEquals(list(1L, 2L, 2L, 3L), eval("(append '(1 2) '(2 3))", env));
    assertEquals(list(1L, 2L, 3L, 4L, 5L), eval("(append '(1) '(2) '(3 4) '(5))", env));
    assertEquals(cons(1L, 2L), eval("(append '() (cons 1 2))", env));
    assertEquals(cons(1L, cons(1L, 2L)), eval("(append '(1) (cons 1 2))", env));
    assertEquals(cons(1L, cons(1L, cons(1L, 2L))), eval("(append '(1 1) (cons 1 2))", env));
    assertEquals(NIL, eval("(append '() '() '() '())", env));
    try {
      eval("(append 1 '())", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: 1", e.getMessage());
    }
    try {
      eval("(append '() '() 5 '())", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: 5", e.getMessage());
    }
  }

  @Test
  public void testReverse() {

    assertEquals(NIL, eval("(reverse '())", env));
    assertEquals(list(1L), eval("(reverse '(1))", env));
    assertEquals(list(1L, 2L, 3L), eval("(reverse '(3 2 1))", env));
    assertEquals(list(1L, 2L, 3L), eval("(reverse (reverse '(1 2 3)))", env));
    try {
      eval("(reverse 1)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: 1", e.getMessage());
    }
    try {
      eval("(reverse '(1 2) '(3 4))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong number of arguments (actual: 2, expected: 1) passed to: reverse", e.getMessage());
    }
  }

  @Test
  public void testListTail() {
    assertEquals(list(3L, 4L), eval("(list-tail (list 1 2 3 4) 2)", env));
    assertEquals(2L, eval("(list-tail (cons 1 2) 1)", env));
    assertEquals(new SCMSymbol("not-a-pair"), eval("(list-tail 'not-a-pair 0)", env));

    eval("(define a '(1 2 3 4))", env);
    eval("(define b (list-tail (cdr a) 2))", env);
    eval("(set-cdr! b '(33 44))", env);
    assertEquals(list(1L, 2L, 3L, 4L, 33L, 44L), eval("a", env));
    assertEquals(list(4L, 33L, 44L), eval("b", env));
    try {
      eval("(list-tail 1 2)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testListRef() {
    assertEquals(1L, eval("(list-ref '(1) 0)", env));
    assertEquals(3L, eval("(list-ref '(1 2 3) 2)", env));
    assertEquals(1L, eval("(list-ref (cons 1 2) 0)", env));
    assertEquals(new SCMSymbol("c"), eval("(list-ref (list 'a 'b 'c) 2)", env));
//  FIXME assertEquals(cons(1L, 2L), eval("(list-ref '(1 2 (1 . 2)) 2)", env));
    assertEquals(list(1L, 2L), eval("(list-ref '(1 2 (1 2)) 2)", env));
    try {
      eval("(list-ref 1 2)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: 1", e.getMessage());
    }
    try {
      eval("(list-ref '(1 2) 2.5)", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Integer, actual: 2.5", e.getMessage());
    }
  }

  @Test
  public void testListToString() {
    assertEquals("", eval("(list->string '())", env));
    assertEquals("AB", eval("(list->string '(#\\A #\\B))", env));
    assertEquals("B", eval("(list->string (cdr '(#\\A #\\B)))", env));
    try {
      eval("(list->string (cons 1 2))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: List, actual: (1 . 2)", e.getMessage());
    }
    try {
      eval("(list->string (list 1 2))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testStringToList() {
    assertEquals(list('a', 'b', 'c'), eval("(string->list \"abc\")", env));
    assertEquals(list('a'), eval("(string->list \"a\")", env));
    assertEquals(list(), eval("(string->list \"\")", env));
    try {
      eval("(string->list (cons 1 2))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: String, actual: (1 . 2)", e.getMessage());
    }
  }

  @Test
  public void testEvalStringRef() {
    assertEquals('t', eval("(string-ref \"test string\" 0)", env));
    assertEquals('e', eval("(string-ref \"test string\" 1)", env));
    assertEquals('s', eval("(string-ref \"test string\" 2)", env));

    try {
      eval("(string-ref \"test\" -1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }
    try {
      eval("(string-ref \"tes\" 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }
    try {
      eval("(string-ref \"\" 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }
    try {
      eval("(string-ref '(1 2 3) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: String, actual: (1 2 3)"));
    }
    try {
      eval("(string-ref \"test\" 0.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
  }

  // FIXME Make modifiable String
  @Test
  public void testEvalStringSet() {
    assertEquals("z", eval("(string-set! \"a\" 0 #\\z)", env));
    assertEquals("zbc", eval("(string-set! \"abc\" 0 #\\z)", env));
    assertEquals("azc", eval("(string-set! \"abc\" 1 #\\z)", env));
    assertEquals("abz", eval("(string-set! \"abc\" 2 #\\z)", env));

    try {
      eval("(string-set! \"abc\" -1 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }
    try {
      eval("(string-set! \"abc\" 3 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }
    try {
      eval("(string-set! \"\" 0 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }
    try {
      eval("(string-set! '(1 2 3) 2 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: String, actual: (1 2 3)"));
    }
    try {
      eval("(string-set! \"test\" 0.5 #\\A)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
    try {
      eval("(string-set! \"test\" 3 '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Character, actual: ()"));
    }
    // FIXME Mutable string?
  }

  @Test
  public void testFlattenList() {

    String flatten = "(define (flatten x)" +
                     "    (cond ((null? x) '())" +
                     "          ((not (pair? x)) (list x))" +
                     "          (else (append (flatten (car x))" +
                     "                        (flatten (cdr x))))))";
    eval(flatten, env);
    assertEquals(list(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
                 eval("(flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))", env));
  }

  @Test
  public void testQuickSort() {

    String splitby = "(define (split-by l p k)" +
                     "  (let loop ((low '())" +
                     "             (high '())" +
                     "             (l l))" +
                     "    (cond ((null? l)" +
                     "           (k low high))" +
                     "          ((p (car l))" +
                     "           (loop low (cons (car l) high) (cdr l)))" +
                     "          (else" +
                     "           (loop (cons (car l) low) high (cdr l))))))";

    String quick = "(define (quicksort l gt?)" +
                   "  (if (null? l)" +
                   "      '()" +
                   "      (split-by (cdr l) " +
                   "                (lambda (x) (gt? x (car l)))" +
                   "                (lambda (low high)" +
                   "                  (append (quicksort low gt?)" +
                   "                          (list (car l))" +
                   "                          (quicksort high gt?))))))";

    eval(splitby, env);
    eval(quick, env);
    assertEquals(list(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
                 eval("(quicksort '(1 3 5 7 9 8 6 4 2) >)", env));
  }

  @Test
  public void testGnomeSort() {

    String gnome = "(define (gnome-sort-compar in-order input-list)" +
                   "  (let gnome ((p (list (car input-list)))" +
                   "              (n (cdr input-list)))" +
                   "    (if (null? n)" +
                   "        p" +
                   "        (let ((prev-pot (car p))" +
                   "              (next-pot (car n)))" +
                   "          (if (in-order next-pot prev-pot)" +
                   "              (gnome (cons next-pot p) (cdr n))" +
                   "              (if (null? (cdr p))" +
                   "                  (gnome (list next-pot) (cons prev-pot (cdr n)))" +
                   "                  (gnome (cdr p) (cons next-pot (cons prev-pot (cdr n))))))))))";
    eval(gnome, env);

    String test = "(gnome-sort-compar <= '(98 36 2 78 5 81 32 90 73 21 94 28 53 25 10 99))";
    SCMCons sorted = list(2L, 5L, 10L, 21L, 25L, 28L, 32L, 36L, 53L, 73L, 78L, 81L, 90L, 94L, 98L, 99L);
    assertEquals(sorted, eval(test, env));
  }

  @Test
  public void testHailstoneSeq() {

    String hailstone = "(define (hailstone n)" +
                       "  (if (= n 1) '(1)" +
                       "  (cons n (hailstone (if (even? n) (/ n 2) (+ 1 (* 3 n)))))))";

    String hailstoneLength = "(define (hailstone-length n)" +
                             "  (let aux ((n n) (r 1)) (if (= n 1) r" +
                             "  (aux (if (even? n) (/ n 2) (+ 1 (* 3 n))) (+ r 1)))))";

    String hailstoneMax = "(define (hailstone-max a b)" +
                          "  (let aux ((i a) (j 0) (k 0))" +
                          "    (if (> i b) (list j k)" +
                          "      (let ((h (hailstone-length i)))" +
                          "        (if (> h k) (aux (+ i 1) i h) (aux (+ i 1) j k))))))";

    eval(hailstone, env);
    eval(hailstoneLength, env);
    eval(hailstoneMax, env);

    SCMCons seq = list(27L, 82L, 41L, 124L, 62L, 31L, 94L, 47L, 142L, 71L, 214L, 107L, 322L, 161L, 484L,
                               242L, 121L, 364L, 182L, 91L, 274L, 137L, 412L, 206L, 103L, 310L, 155L, 466L, 233L,
                               700L, 350L, 175L, 526L, 263L, 790L, 395L, 1186L, 593L, 1780L, 890L, 445L, 1336L,
                               668L, 334L, 167L, 502L, 251L, 754L, 377L, 1132L, 566L, 283L, 850L, 425L, 1276L,
                               638L, 319L, 958L, 479L, 1438L, 719L, 2158L, 1079L, 3238L, 1619L, 4858L, 2429L,
                               7288L, 3644L, 1822L, 911L, 2734L, 1367L, 4102L, 2051L, 6154L, 3077L, 9232L, 4616L,
                               2308L, 1154L, 577L, 1732L, 866L, 433L, 1300L, 650L, 325L, 976L, 488L, 244L, 122L,
                               61L, 184L, 92L, 46L, 23L, 70L, 35L, 106L, 53L, 160L, 80L, 40L, 20L, 10L, 5L, 16L,
                               8L, 4L, 2L, 1L);
    assertEquals(seq, eval("(hailstone 27)", env));
    assertEquals(112L, eval("(hailstone-length 27)", env));
  }

  @Test
  public void testEvalHornersRule() {
    String horner = "(define (horner lst x)" +
                    "  (define (*horner lst x acc)" +
                    "    (if (null? lst)" +
                    "        acc" +
                    "        (*horner (cdr lst) x (+ (* acc x) (car lst)))))" +
                    "  (*horner (reverse lst) x 0))";
    eval(horner, env);
    assertEquals(128L, eval("(horner '(-19 7 -4 6) 3)", env));
  }

  @Test
  public void testEvalDisplay() {

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintStream old = System.out;
    System.setOut(new PrintStream(baos));

    IEnvironment tempEnv = new DefaultEnvironment();
    /* Eval lib procedures */
    for (String proc : tempEnv.getLibraryProcedures()) {
      eval(proc, tempEnv);
    }
    tempEnv.put(new SCMSymbol("display"), new Display(System.out));

    eval("(display 123)", tempEnv);
    assertEquals("123", baos.toString().trim());
    baos.reset();

    eval("(display -123.25)", tempEnv);
    assertEquals("-123.25", baos.toString().trim());
    baos.reset();

    eval("(display \"test string\")", tempEnv);
    assertEquals("test string", baos.toString().trim());
    baos.reset();

    eval("(display '())", tempEnv);
    assertEquals("()", baos.toString().trim());
    baos.reset();

    eval("(display '(1 2 3 #\\A (1 . 2)))", tempEnv);
    assertEquals("(1 2 3 #\\A (1 . 2))", baos.toString().trim());
    baos.reset();

    eval("(display (list 1 2 3 #\\A (cons 1 2) (list 1 2 3)))", tempEnv);
    assertEquals("(1 2 3 #\\A (1 . 2) (1 2 3))", baos.toString().trim());
    baos.reset();

    eval("(display (string->list \"Hello\"))", tempEnv);
    assertEquals("(#\\H #\\e #\\l #\\l #\\o)", baos.toString().trim());
    baos.reset();

    eval("(display (cdr (cdr '(1 2 3 4 5 6))))", tempEnv);
    assertEquals("(3 4 5 6)", baos.toString().trim());
    baos.reset();

    System.setOut(old);
  }

  @Test
  public void testEvalGroupNumbers() {
    String group = "(let loop ((numbers '(3 -2 1 6 -5))" +
                   "           (nonneg '())" +
                   "           (neg '()))" +
                   "  (cond ((null? numbers) (list nonneg neg))" +
                   "        ((>= (car numbers) 0)" +
                   "         (loop (cdr numbers)" +
                   "               (cons (car numbers) nonneg)" +
                   "               neg))" +
                   "        ((< (car numbers) 0)" +
                   "         (loop (cdr numbers)" +
                   "               nonneg" +
                   "               (cons (car numbers) neg)))))";

    assertEquals(list(list(6L, 1L, 3L), list(-5L, -2L)), eval(group, env));
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

  /* Helper method */
  private Object eval(String sexp, IEnvironment env) {
    return eval.eval(reader.read(sexp), env);
  }
}
