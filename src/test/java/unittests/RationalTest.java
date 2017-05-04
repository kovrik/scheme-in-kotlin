package unittests;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class RationalTest extends AbstractTest {

  @Test
  public void testZero() {
    assertEquals(0L, eval("0/1", env));
    assertEquals(0L, eval("0000/1111", env));
    assertEquals(0L, eval("-0000/1111", env));
    try {
      eval("1/0", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("/ by zero", e.getMessage());
    }
  }

  @Test
  public void testOne() {
    assertEquals(1L, eval("1/1", env));
    assertEquals(1L, eval("1111/1111", env));
    assertEquals(1L, eval("12345/12345", env));
  }
}
