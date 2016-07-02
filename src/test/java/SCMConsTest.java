import org.junit.Test;
import static org.junit.Assert.assertEquals;

import static core.scm.SCMCons.cons;

public class SCMConsTest {

  @Test
  public void tetToString() {

    assertEquals("(1 . 2)", cons(1, 2).toString());
    assertEquals("(1 2 . 3)", cons(1, cons(2, 3)).toString());
    assertEquals("(1 2 3 . 4)", cons(1, cons(2, cons(3, 4))).toString());
  }
}
