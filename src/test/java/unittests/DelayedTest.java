package unittests;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.exceptions.ReentrantPromiseException;
import core.reader.IReader;
import core.reader.Reader;
import org.junit.Before;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class DelayedTest {

  private final IReader reader = new Reader();
  private final IEvaluator eval = new Evaluator();
  private final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (String proc : env.getLibraryProcedures()) {
      eval(proc, env);
    }
  }

  /* Helper method */
  private Object eval(String sexp, IEnvironment env) {
    return eval.eval(reader.read(sexp), env);
  }

  @Before
  public void setUp() throws Exception {
    // TODO Create new environment for each test?
  }

  @Test
  public void testEvalDelay() {
    assertEquals(TRUE,  eval("(promise?   (delay (* (+ 2 3) 4))))", env));
    assertEquals(FALSE, eval("(procedure? (delay (* (+ 2 3) 4))))", env));
  }

  @Test
  public void testEvalPromise() {
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
  public void testReentrantPromise() {
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
    } catch (ReentrantPromiseException e) {
      assertTrue(e.getMessage().startsWith("Reentrant promise:"));
    }
  }
}
