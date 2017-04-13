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
    assertEquals(false, eval("(let ((p (promise))) (realized? p))", env));
    assertEquals(false, eval("(let ((p (promise))) (future?   p))", env));
    assertEquals(false, eval("(let ((p (promise))) (delay?    p))", env));
    assertEquals(true,  eval("(let ((p (promise))) (promise?  p))", env));
    assertEquals(true,  eval("(let ((p (promise))) (deliver p 1) (realized? p))", env));
    assertEquals(12L,   eval("(let ((p (promise))) (deliver p (+ 1 2 3)) (+ @p (deref p)))", env));
  }

  @Test
  public void testDelay() {
    assertEquals(false, eval("(let ((d (delay (+ 1 2 3)))) (realized? d))", env));
    assertEquals(false, eval("(let ((d (delay (+ 1 2 3)))) (future?   d))", env));
    assertEquals(true,  eval("(let ((d (delay (+ 1 2 3)))) (delay?    d))", env));
    assertEquals(true,  eval("(let ((d (delay (+ 1 2 3)))) (promise?  d))", env));
    assertEquals(true,  eval("(let ((d (delay (+ 1 2 3)))) @d (realized? d))", env));
    assertEquals(6L,    eval("(let ((d (delay (+ 1 2 3)))) @d)", env));
  }

  @Test
  public void testFuture() {
    assertEquals(true,  eval("(let ((f (future (+ 1 2 3)))) (future?   f))", env));
    assertEquals(false, eval("(let ((f (future (+ 1 2 3)))) (delay?    f))", env));
    assertEquals(false, eval("(let ((f (future (+ 1 2 3)))) (promise?  f))", env));
    assertEquals(6L,    eval("(let ((f (future (+ 1 2 3)))) @f)", env));
    assertEquals(true,  eval("(let ((f (future (+ 1 2 3)))) @f (future-done? f))", env));
    assertEquals(false, eval("(let ((f (future (future-cancel f)))) (future-done? f))", env));
  }
}
