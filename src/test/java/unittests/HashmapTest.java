package unittests;

import core.exceptions.ArityException;
import core.scm.Cons;
import org.junit.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class HashmapTest extends AbstractTest {

  @Test
  public void testHashmap() {
    assertTrue(eval("{}", env) instanceof Map);
    assertTrue(eval("{1 2}", env) instanceof Map);
    assertTrue(eval("{1 2, 3 4}", env) instanceof Map);
    assertTrue(eval("{1,2,3,4}", env) instanceof Map);
    assertTrue(eval("{1,2,,3,4}", env) instanceof Map);
    assertTrue(eval("{1 2, 3 4 ,   5   8 }", env) instanceof Map);
    assertTrue(eval("(hash-map 1 2 3 4    5   8 )", env) instanceof Map);
  }

  @Test
  public void testHashmapGet() {
    assertEquals(5L, eval("({3 5} 3)", env));
    assertEquals(5L, eval("({3 5} 7 5)", env));
    assertEquals(5L, eval("(get {3 5} 3)", env));
    assertEquals(5L, eval("(get {3 5} 7 5)", env));
    assertEquals("B", eval("({3 5, \"A\" \"B\"} \"A\" 5)", env));
    assertEquals("B", eval("((hash-map 3 5 \"A\" \"B\") \"A\" 5)", env));
    assertEquals("B", eval("(get {3 5, \"A\" \"B\"} \"A\" 5)", env));
    assertEquals("B", eval("(get (hash-map 3 5 \"A\" \"B\") \"A\" 5)", env));

    assertEquals(5L, eval("(.get {3 5} 3)", env));
    assertEquals(5L, eval("(.getOrDefault {3 5} 7 5)", env));
    assertEquals("B", eval("(.getOrDefault {3 5, \"A\" \"B\"} \"A\" 5)", env));
    assertEquals("B", eval("(.getOrDefault (hash-map 3 5 \"A\" \"B\") \"A\" 5)", env));
  }

  @Test
  public void testHashmapPut() {
    assertEquals(5L, eval("((put {} 3 5) 3)", env));
    assertEquals(5L, eval("((put {} 3 (+ 2 3)) 7 5)", env));
    assertEquals(5L, eval("(get (put {} 3 (+ 2 3)) (+ 1 2))", env));
    assertEquals(5L, eval("(get (put {} 3 (+ 2 3)) 7 5)", env));
    assertEquals("B", eval("((put {} 3 5 \"A\" \"B\") \"A\" 5)", env));
    assertEquals("B", eval("((put (hash-map) 3 5 \"A\" \"B\") \"A\" 5)", env));
    assertEquals("B", eval("(get (put {} 3 5 \"A\" \"B\") \"A\" 5)", env));
    assertEquals("B", eval("(get (put (hash-map) 3 5 \"A\" \"B\") \"A\" 5)", env));
  }

  @Test
  public void testHashmapEval() {
    assertEquals(8L, eval("({(+ 1 2 3) (* 2 4)} 6)", env));
    assertEquals(8L, eval("((hash-map (+ 1 2 3) (* 2 4)) 6)", env));
    assertEquals(10L, eval("(({* *, + +} +) 1 2 3 4)", env));
    assertEquals(10L, eval("(((hash-map * * + +) +) 1 2 3 4)", env));
    try {
      eval("({:test true} 1 2 3)", env);
      fail();
    } catch (ArityException e) {
      // success
    }
  }

  @Test
  public void testHashmapKeysValues() {
    assertEquals(Cons.list(1L, 2L, 3L), eval("(keys {1 + 2 - 3 /})", env));
    assertEquals(Cons.EMPTY, eval("(keys {})", env));
    assertEquals(6L, eval("(apply + (vals (zipmap '[+ - /] '(1 2 3))))", env));
    assertEquals(Cons.EMPTY, eval("(vals {})", env));
    assertEquals(Collections.EMPTY_MAP, eval("(zipmap [] '())", env));
    assertEquals(Collections.EMPTY_MAP, eval("(zipmap nil nil)", env));
    assertEquals(3L, eval("(get (zipmap \"test\" [1 2 3 4]) #\\s)", env));
  }
}
