package unittests.s7.tests;

import org.junit.Test;
import unittests.AbstractTest;

import static junit.framework.TestCase.assertEquals;
import static java.lang.Boolean.TRUE;
import static java.lang.Boolean.FALSE;

public class NotTest extends AbstractTest {

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
