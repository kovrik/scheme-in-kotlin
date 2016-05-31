import core.environment.DefaultEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.parser.IParser;
import core.parser.Tokenizer;
import core.procedures.delayed.SCMPromise;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
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
    // FIXME
//    assertEquals(0.001, eval.eval(tokenizer.parse("(/ 1 10 10)"), env));
  }

  @Test
  public void testEvalNumericalComparison() {

    assertEquals(TRUE, eval.eval(tokenizer.parse("(= 1 1 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(= 1 0 1)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(= 0)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(= 0.57 0.5700)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(= 7 7.00)"), env));

    assertEquals(TRUE, eval.eval(tokenizer.parse("(> 2 1)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(> 2 1.123)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(>= 2 1.123)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(>= 2.5 1.123)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(<= -2.5 1.123)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(< -2.5 1.123)"), env));
  }

  @Test
  public void testEvalNegation() {

    assertEquals(FALSE, eval.eval(tokenizer.parse("(not #t)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(not #f)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(not (= 1 2 1))"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(not (= 1 1 1))"), env));
  }

  // Equivalence
  @Test
  public void testEvalCharEq() {

    assertEquals(TRUE, eval.eval(tokenizer.parse("(char=? #\\A #\\A)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(char=? #\\B #\\A)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(char=? #\\newline #\\newline)"), env));
  }

  @Test
  public void testEvalCharEqCi() {

    assertEquals(TRUE, eval.eval(tokenizer.parse("(char-ci=? #\\Z #\\z)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(char-ci=? #\\b #\\A)"), env));
  }

  @Test
  public void testEvalStringEq() {

    assertEquals(TRUE, eval.eval(tokenizer.parse("(string=? \"test\" \"test\")"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(string=? \"test\" \"test123\")"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(string=? \"\" \"\")"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(string=? \"test\" \"Test\")"), env));
  }

  @Test
  public void testEvalStringEqCi() {

    assertEquals(TRUE, eval.eval(tokenizer.parse("(string-ci=? \"test\" \"test\")"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(string-ci=? \"test\" \"test123\")"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(string-ci=? \"\" \"\")"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(string-ci=? \"test\" \"Test\")"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(string-ci=? \"tESt\" \"TesT\")"), env));
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
    assertEquals(FALSE,  eval.eval(tokenizer.parse("(equal? '(1 2 3 5) '( 1 2 3))"), env));
    assertEquals(TRUE,  eval.eval(tokenizer.parse("(equal? 1 1)"), env));
    assertEquals(FALSE, eval.eval(tokenizer.parse("(equal? 1 2)"), env));
    assertEquals(TRUE, eval.eval(tokenizer.parse("(equal? \"1fe\" \"1fe\")"), env));
  }

  @Test
  public void testEvalDelayed() {

    assertEquals(1d, eval.eval(tokenizer.parse("(force (delay 1.0))"), env));
    assertEquals("test", eval.eval(tokenizer.parse("(force (delay \"test\"))"), env));
    assertEquals(10L, eval.eval(tokenizer.parse("(force (delay (+ 5 2 (* 1 3))))"), env));

    assertEquals(SCMPromise.class, eval.eval(tokenizer.parse("(delay 1.0)"), env).getClass());
    assertEquals(TRUE, eval.eval(tokenizer.parse("(promise? (delay 1.0))"), env));
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

  // Procedures
  @Test
  public void testEvalProcedure() {

    assertEquals(SCMProcedure.class, eval.eval(tokenizer.parse("(lambda () #t)"), env).getClass());
    assertEquals(TRUE, eval.eval(tokenizer.parse("((lambda () #t))"), env));
    assertEquals(6L, eval.eval(tokenizer.parse("((lambda (n) (+ n 1)) 5)"), env));

    eval.eval(tokenizer.parse("(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"), env);
    assertEquals(8L, eval.eval(tokenizer.parse("(fib 5)"), env));

    assertEquals(6L, eval.eval(tokenizer.parse("((lambda (n) (+ n 1)) 5)"), env));
  }

  // TODO
  // Special Forms
  @Test
  public void testEvalSpecialForms() {

    // define
    eval.eval(tokenizer.parse("(define a 5)"), env);
    assertEquals(5L, eval.eval(tokenizer.parse("a"), env));
    assertEquals(SCMSpecialForm.DEFINE, eval.eval(tokenizer.parse("(define b 7)"), env));

    // lambda
    String f1 = "(lambda ())";
    try {
      eval.eval(tokenizer.parse(f1), env);
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("bad lambda in form: " + f1));
    }

    // if
    assertEquals(5L, eval.eval(tokenizer.parse("(if #t 5 0)"), env));
    assertEquals(5L, eval.eval(tokenizer.parse("(if #f 0 5)"), env));
    assertEquals(0L, eval.eval(tokenizer.parse("(if (not #f) 0 5)"), env));
    assertEquals(5L, eval.eval(tokenizer.parse("(if (not (not (or #f #f))) 0 (+ 3 2))"), env));

    // when
    assertEquals(0L, eval.eval(tokenizer.parse("(when #t 5 4 3 2 1 0)"), env));
    assertEquals(null, eval.eval(tokenizer.parse("(when #f 5 4 3 2 1 0)"), env));

    // quote
    assertEquals(0L, eval.eval(tokenizer.parse("'0"), env));
    assertEquals("test", eval.eval(tokenizer.parse("'\"test\""), env));
    assertEquals(new SCMList<Object>(SCMSpecialForm.QUOTE, "test"), eval.eval(tokenizer.parse("''\"test\""), env));
    assertEquals(new SCMList<Object>(new SCMSymbol("+"), 1L, 2L), eval.eval(tokenizer.parse("'(+ 1 2)"), env));

    // set!
    assertEquals(9L, eval.eval(tokenizer.parse("(let ((a 0)) (set! a 9) a)"), env));
    assertEquals(99L, eval.eval(tokenizer.parse("(begin (set! b 99) b)"), env));

    // let
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

    // let*
    assertEquals(2L, eval.eval(tokenizer.parse("(let* ((z 1) (b (+ z 1))) b)"), env));
    try {
      eval.eval(tokenizer.parse("(let* ((c 123)))"), env);
    } catch (IllegalArgumentException e) {
      assertTrue("Test bad let* form", e.getMessage().contains("let*: bad let* in form:"));
    }

    // letrec
    String letrec1 = "(letrec ((is-even? (lambda (n) (or (= n 0) (is-odd? (- n 1))))) " +
                              "(is-odd?  (lambda (n) (and (not (= n 0)) (is-even? (- n 1))))))" +
                     "  (is-odd? 11))";
    assertEquals(TRUE, eval.eval(tokenizer.parse(letrec1), env));

    // cond
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

    // case

  }

  // TODO Exceptions
}
