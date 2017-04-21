package unittests.s7.tests;

import org.junit.Test;
import unittests.AbstractTest;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;

public class NotTest extends AbstractTest {

  @Test
  public void testNot() {
    assertEquals(TRUE,  eval("(not #f)", env));
    assertEquals(TRUE,  eval("(not (not #t))", env));

    String[] falses = {"(not #t)", "(not 0)", "(not 1)", "(not '())", "(not 't)", "(not (list))",
        "(not (list 3))", "(not 'nil)", "(not not)", "(not \"\")", "(not 'lambda)", "(not 'quote)",
        "(not 'and)", "(not 'case)",};
    assertAllEqual(FALSE, falses, env);
  }
}
