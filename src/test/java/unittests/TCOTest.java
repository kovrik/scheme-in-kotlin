package unittests;

import core.scm.SCMMutableString;
import org.junit.Test;

import static core.scm.SCMBoolean.TRUE;
import static org.junit.Assert.assertEquals;

public class TCOTest extends AbstractTest {

  private final Long iterations = 100000L;

  @Test
  public void testIfTCO() {
    String recursive = "(define (recursive n)" +
                       "  (if (zero? n)" +
                       "      \"DONE\"" +
                       "      (recursive (- n 1))))";
    eval(recursive, env);

    assertEquals(new SCMMutableString("DONE"), eval("(recursive 5)", env));
    assertEquals(new SCMMutableString("DONE"), eval("(recursive " + iterations + ")", env));
  }

  @Test
  public void testOrTCO() {
    String recursive = "(define (recOr n) (or (zero? n) (recOr (- n 1))))";
    eval(recursive, env);

    assertEquals(TRUE, eval("(recOr 5)", env));
    assertEquals(TRUE, eval("(recOr " + iterations + ")", env));
  }

  @Test
  public void testDefineAndBeginTCO() {
    String recursive = "(define (foo n) (if (= n " + iterations+ ") n (foo (+ n 1)))";
    eval(recursive, env);
    assertEquals(iterations, eval("(foo 5)", env));
  }

  // TODO Tests for other Special Forms
}
