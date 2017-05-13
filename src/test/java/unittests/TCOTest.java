package unittests;

import org.junit.Test;

import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;

public class TCOTest extends AbstractTest {

  private static final long ITERS = 100000L;

  @Test
  public void testIfTCO() {
    String recur = "(define (recur n)" +
                   "  (if (zero? n)" +
                   "      \"DONE\"" +
                   "      (recur (- n 1))))";
    eval(recur, env);
    assertEquals("DONE", eval("(recur " + ITERS + ")", env));
  }

  @Test
  public void testOrTCO() {
    String recursive = "(define (recOr n) (or (zero? n) (recOr (- n 1))))";
    eval(recursive, env);
    assertEquals(TRUE, eval("(recOr " + ITERS + ")", env));
  }

  @Test
  public void testDefineAndBeginTCO() {
    String recursive = "(define (foo n) (if (= n " + ITERS+ ") n (foo (+ n 1)))";
    eval(recursive, env);
    assertEquals(ITERS, eval("(foo 5)", env));
  }

  // TODO Tests for other Special Forms
}
