package s7.tests;

import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static junit.framework.TestCase.assertEquals;

public class NotTest extends AbstractS7Test {

  @Test
  public void testNot() {
    assertEquals(TRUE,  eval("(not #f)", env));
    assertEquals(FALSE, eval("(not #t)", env));
    assertEquals(TRUE,  eval("(not (not #t))", env));
    assertEquals(FALSE, eval("(not 0)", env));
    assertEquals(FALSE, eval("(not 1)", env));
    assertEquals(FALSE, eval("(not '())", env));
    assertEquals(FALSE, eval("(not 't)", env));
    assertEquals(FALSE, eval("(not (list))", env));
    assertEquals(FALSE, eval("(not (list 3))", env));
    assertEquals(FALSE, eval("(not 'nil)", env));
    assertEquals(FALSE, eval("(not not)", env));
    assertEquals(FALSE, eval("(not \"\")", env));
    assertEquals(FALSE, eval("(not 'lambda)", env));
    assertEquals(FALSE, eval("(not 'quote)", env));
    assertEquals(FALSE, eval("(not 'and)", env));
    assertEquals(FALSE, eval("(not 'case)", env));
  }
}
