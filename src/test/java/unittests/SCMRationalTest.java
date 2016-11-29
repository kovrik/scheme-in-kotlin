package unittests;

import core.scm.SCMBigRational;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class SCMRationalTest extends AbstractTest {

  @Test
  public void testZero() {
    assertEquals(SCMBigRational.ZERO, eval("0/1", env));
    assertEquals(SCMBigRational.ZERO, eval("0000/1111", env));
    assertEquals(SCMBigRational.ZERO, eval("-0000/1111", env));

    try {
      eval("1/0", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("/ by zero", e.getMessage());
    }
  }
}