package unittests;

import core.scm.SCMCons;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.Collections;

import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class JavaInteropTest extends AbstractTest {

  @Test
  public void testJavaStaticFields() {
    assertEquals(Math.PI, eval("Math/PI", env));
    assertTrue(Collections.EMPTY_LIST == eval("java.util.Collections/EMPTY_LIST", env));
    assertTrue(SCMCons.EMPTY == eval(SCMCons.class.getName() + "/EMPTY", env));
  }

  @Test
  public void testJavaStaticMethods() {
    System.setProperty("TESTKEY", "TESTVALUE");
    assertEquals("TESTVALUE", eval("(System/getProperty \"TESTKEY\")", env));
    assertEquals(5L, eval("(Short/parseShort \"12\" 3)", env));
//    assertEquals(3L, eval("(Short/valueOf 3)", env));
  }

  @Test
  public void testJavaInstanceMethods() {
    assertEquals("FRED", eval("(.toUpperCase \"fred\")", env));
    assertEquals(Long.class, eval("(.getClass 5)", env));
    assertEquals(Class.class, eval("(.getClass String)", env));
    assertEquals(String.class, eval("(.getClass \"String\")", env));
    String map = "(define h (new java.util.HashMap))";
    eval(map, env);
    eval("(.put h \"KEY\" \"VALUE\")", env);
    assertEquals("VALUE", eval("(.get h \"KEY\")", env));
    assertEquals(5L, eval("(.length (.get h \"KEY\"))", env));
    eval("(.put h 1 1)", env);
    assertEquals(1L, eval("(.get h 1)", env));
  }

  @Test
  public void testJavaClassMethods() {
    assertEquals("ring", eval("(.substring \"String\" 2)", env));
    assertEquals("java.lang.String", eval("(.getName String)", env));
    assertEquals("java.util.Collections", eval("(.getName java.util.Collections)", env));
  }

  @Test
  public void testJavaDowncast() {
    assertEquals("es", eval("(.substring \"test\" 1 3)", env));
    assertEquals("zesz", eval("(.replace \"test\" #\\t #\\z)", env));
    assertEquals(TRUE, eval("(.isEmpty \"\")", env));
    assertEquals('e', eval("(.charAt \"test\" 1)", env));
    assertEquals(4L, eval("(.length \"test\")", env));
  }

  @Test
  public void testJavaNewInstance() {
    assertEquals(1L, eval("(new Long 1)", env));
    assertEquals(6L, eval("(new Long (+ 1 2 3))", env));
    assertEquals(31L, eval("(new Long (.substring \"123123\" 2 4))", env));
    assertEquals(BigDecimal.ONE, eval("(new java.math.BigDecimal 1)", env));
    assertEquals(123, eval("(new Integer 123)", env));
    // FIXME
//    assertEquals(123, eval("(new Short 123)", env));
//    (.signum (new java.math.BigDecimal 10))
  }
}
