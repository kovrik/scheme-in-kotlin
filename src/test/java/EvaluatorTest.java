import core.environment.DefaultEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.parser.IParser;
import core.parser.Tokenizer;
import core.procedures.delayed.SCMPromise;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static org.junit.Assert.assertEquals;

public class EvaluatorTest {

  private final IParser tokenizer = new Tokenizer();
  private final IEvaluator evaluator = new Evaluator();
  private final DefaultEnvironment defaultEnvironment = new DefaultEnvironment();

  @Test
  public void testEvalNumbers() {

    assertEquals(1L, evaluator.eval(tokenizer.parse("1"), defaultEnvironment));
    assertEquals(-15L, evaluator.eval(tokenizer.parse("-15"), defaultEnvironment));
    assertEquals(-2.5d, evaluator.eval(tokenizer.parse("-2.5"), defaultEnvironment));
  }

  @Test
  public void testEvalStrings() {

    assertEquals("1", evaluator.eval(tokenizer.parse("\"1\""), defaultEnvironment));
    assertEquals("Lorem ipsum", evaluator.eval(tokenizer.parse("\"Lorem ipsum\""), defaultEnvironment));
    assertEquals("Lorem \\\"ipsum\\\" ", evaluator.eval(tokenizer.parse("\"Lorem \\\"ipsum\\\" \""), defaultEnvironment));
    assertEquals("", evaluator.eval(tokenizer.parse("\"\""), defaultEnvironment));
  }

  @Test
  public void testEvalMath() {

    assertEquals(6L,  evaluator.eval(tokenizer.parse("(+ 1 2 3)"), defaultEnvironment));
    assertEquals(5.5, evaluator.eval(tokenizer.parse("(/ (+ 1 2 3 (- (* 2 2.5 2) 5)) 2)"), defaultEnvironment));
    assertEquals(5.0, evaluator.eval(tokenizer.parse("(/ 10.0 2)"), defaultEnvironment));
    assertEquals(0.1, evaluator.eval(tokenizer.parse("(/ 10)"), defaultEnvironment));
    // FIXME
//    assertEquals(0.001, evaluator.eval(tokenizer.parse("(/ 1 10 10)"), defaultEnvironment));
  }

  @Test
  public void testEvalNumericalComparison() {

    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(= 1 1 1)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(= 1 0 1)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(= 0)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(= 0.57 0.5700)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(= 7 7.00)"), defaultEnvironment));

    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(> 2 1)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(> 2 1.123)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(>= 2 1.123)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(>= 2.5 1.123)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(<= -2.5 1.123)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(< -2.5 1.123)"), defaultEnvironment));
  }

  @Test
  public void testEvalNegation() {

    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(not #t)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(not #f)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(not (= 1 2 1))"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(not (= 1 1 1))"), defaultEnvironment));
  }

  // Equivalence
  @Test
  public void testEvalCharEq() {

    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(char=? #\\A #\\A)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(char=? #\\B #\\A)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(char=? #\\newline #\\newline)"), defaultEnvironment));
  }

  @Test
  public void testEvalCharEqCi() {

    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(char-ci=? #\\Z #\\z)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(char-ci=? #\\b #\\A)"), defaultEnvironment));
  }

  @Test
  public void testEvalStringEq() {

    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(string=? \"test\" \"test\")"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(string=? \"test\" \"test123\")"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(string=? \"\" \"\")"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(string=? \"test\" \"Test\")"), defaultEnvironment));
  }

  @Test
  public void testEvalStringEqCi() {

    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(string-ci=? \"test\" \"test\")"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(string-ci=? \"test\" \"test123\")"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(string-ci=? \"\" \"\")"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(string-ci=? \"test\" \"Test\")"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(string-ci=? \"tESt\" \"TesT\")"), defaultEnvironment));
  }

  @Test
  public void testEvalEq() {

    assertEquals(TRUE,  evaluator.eval(tokenizer.parse("(eq? '() '())"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(eq? 1 1)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(eq? 1 2)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(eq? \"1\" \"1\")"), defaultEnvironment));
  }

  @Test
  public void testEvalEqv() {

    assertEquals(TRUE,  evaluator.eval(tokenizer.parse("(eqv? '() '())"), defaultEnvironment));
    assertEquals(TRUE,  evaluator.eval(tokenizer.parse("(eqv? 1 1)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(eqv? 1 2)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(eqv? \"1\" \"1\")"), defaultEnvironment));
  }

  @Test
  public void testEvalEqual() {

    assertEquals(TRUE,  evaluator.eval(tokenizer.parse("(equal? '() '())"), defaultEnvironment));
    assertEquals(TRUE,  evaluator.eval(tokenizer.parse("(equal? '(1 2 3) '( 1 2 3))"), defaultEnvironment));
    assertEquals(FALSE,  evaluator.eval(tokenizer.parse("(equal? '(1 2 3 5) '( 1 2 3))"), defaultEnvironment));
    assertEquals(TRUE,  evaluator.eval(tokenizer.parse("(equal? 1 1)"), defaultEnvironment));
    assertEquals(FALSE, evaluator.eval(tokenizer.parse("(equal? 1 2)"), defaultEnvironment));
    assertEquals(TRUE, evaluator.eval(tokenizer.parse("(equal? \"1fe\" \"1fe\")"), defaultEnvironment));
  }

  @Test
  public void testEvalDelayed() {

    assertEquals(1d, evaluator.eval(tokenizer.parse("(force (delay 1.0))"), defaultEnvironment));
    assertEquals("test", evaluator.eval(tokenizer.parse("(force (delay \"test\"))"), defaultEnvironment));
    assertEquals(10L, evaluator.eval(tokenizer.parse("(force (delay (+ 5 2 (* 1 3))))"), defaultEnvironment));

    assertEquals(SCMPromise.class, evaluator.eval(tokenizer.parse("(delay 1.0)"), defaultEnvironment).getClass());
  }

  // TODO
  // Characters
  // Strings
  // Vectors
  // Special Forms
  // Procedures
}
