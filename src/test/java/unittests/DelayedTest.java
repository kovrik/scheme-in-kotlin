package unittests;

import core.exceptions.ReentrantDelayException;
import org.junit.Test;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.*;

public class DelayedTest extends AbstractTest {

  @Test
  public void testEvalDelay() {
    assertEquals(TRUE,  eval("(promise?   (delay (* (+ 2 3) 4))))", env));
    assertEquals(FALSE, eval("(procedure? (delay (* (+ 2 3) 4))))", env));
  }

  @Test
  public void testEvalDelayed() {
    try {
      eval("((delay (* (+ 2 3) 4))))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().startsWith("Wrong type to apply"));
    }
  }

  @Test
  public void testEvalForce() {
    assertEquals(TRUE, eval("(force (delay (= (+ 1 2) 3)))", env));
  }

  @Test
  public void testReentrantDelay() {
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
      assertTrue(e.getMessage().startsWith("Re-entrant delay:"));
    }
  }

  @Test
  public void testPromise() {
    assertEquals(12L, eval("(let ((p (promise))) (deliver p (+ 1 2 3)) (+ @p (deref p)))", env));
  }
}
