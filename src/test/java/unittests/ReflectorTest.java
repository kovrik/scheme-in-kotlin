package unittests;

import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ReflectorTest extends AbstractTest {

  @Test
  public void testEvalStaticMethods() {
    assertTrue(((List)eval("(java.util.Collections/emptyList)", env)).isEmpty());
    assertTrue(((List)eval("(. java.util.Collections emptyList)", env)).isEmpty());
    assertEquals(-15L, eval("(Long/valueOf -15)", env));
    assertEquals(-15L, eval("(. Long valueOf -15)", env));
    // TODO
  }

  @Test
  public void testEvalStaticFields() {
    assertEquals(BigDecimal.ONE, eval("java.math.BigDecimal/ONE", env));
    assertEquals(BigDecimal.ONE, eval("BigDecimal/ONE", env));
    assertEquals(BigInteger.ONE, eval("BigInteger/ONE", env));
    assertEquals(Math.PI, eval("Math/PI", env));
    assertEquals(Math.PI, eval("(. java.lang.Math PI)", env));
    // TODO
  }

  @Test
  public void testEvalMemberFields() {
    // TODO getting instance fields directly (.-field instance)
    // TODO
  }

  @Test
  public void testEvalMemberMethods() {
    assertEquals(String.class, eval("(.getClass \"\")", env));
    assertEquals(String.class, eval("(. \"\" getClass)", env));
    assertEquals(Long.class, eval("(.getClass 1)", env));
    assertEquals(Long.class, eval("(. 1 getClass)", env));
    assertEquals(Long.class, eval("(. (+ 2 3 4) getClass)", env));
    assertEquals("123", eval("(.toString 123)", env));
    assertEquals(1L, eval("(. (+ 1 2 3) compareTo (+ 1 2))", env));
    try {
      eval("(.getClass nil)", env);
      fail();
    } catch (NullPointerException e) {
      // expected
    }
    try {
      eval("(.toString nil)", env);
      fail();
    } catch (NullPointerException e) {
      // expected
    }
    // TODO
  }

  @Test
  public void testEvalConstructors() {
    assertTrue(eval("(new Object)", env) != null);
    assertTrue(eval("(Object.)", env) != null);
    assertEquals("123", eval("(new String \"123\")", env));
    assertEquals("123", eval("(String. \"123\")", env));
    // TODO
  }
}
