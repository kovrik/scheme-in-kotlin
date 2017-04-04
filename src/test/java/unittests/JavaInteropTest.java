package unittests;

import core.scm.SCMCons;
import org.junit.Test;

import java.util.Collections;

import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class JavaInteropTest extends AbstractTest {

  @Test
  public void testJavaStaticFields() {
    assertEquals(Math.PI, eval("Math/PI", env));
    assertTrue(Collections.EMPTY_LIST == eval("java.util.Collections/EMPTY_LIST", env));
    assertTrue(SCMCons.NIL == eval(SCMCons.class.getName() + "/NIL", env));
  }

  @Test
  public void testJavaInstanceMethods() {
    assertEquals("FRED", eval("(.toUpperCase \"fred\")", env));
    assertEquals(Long.class, eval("(.getClass 5)", env));
  }

  @Test
  public void testJavaClassMethods() {
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
}
