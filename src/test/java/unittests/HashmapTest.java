package unittests;

import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class HashmapTest extends AbstractTest {

  @Test
  public void testHashmap() {
    assertTrue(eval("{}", env) instanceof Map);
    assertTrue(eval("{1 2}", env) instanceof Map);
    assertTrue(eval("{1 2, 3 4}", env) instanceof Map);
    assertTrue(eval("{1 2, 3 4 ,   5   8 }", env) instanceof Map);
  }

  @Test
  public void testHashmapGet() {
    assertEquals(5L, eval("({3 5} 3)", env));
    assertEquals(5L, eval("({3 5} 7 5)", env));
    assertEquals("B", eval("({3 5, \"A\" \"B\"} \"A\" 5)", env));
  }

  @Test
  public void testHashmapEval() {
    assertEquals(8L, eval("({(+ 1 2 3) (* 2 4)} 6)", env));
    assertEquals(10L, eval("(({* *, + +} +) 1 2 3 4)", env));
  }
}
