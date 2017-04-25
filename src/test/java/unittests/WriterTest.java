package unittests;

import core.scm.SCMCons;
import core.scm.SCMNil;
import core.writer.IWriter;
import core.writer.Writer;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static core.scm.SCMCons.EMPTY;
import static org.junit.Assert.assertEquals;

public class WriterTest {

  private final IWriter writer = new Writer();

  @Test
  public void testWriteString() {
    assertEquals("\"test string\"", writer.toString("test string"));
    assertEquals("\"\"", writer.toString(""));
    assertEquals("\"(1 2 3)\"", writer.toString("(1 2 3)"));
  }

  @Test
  public void testWriteChar() {
    assertEquals("#\\a", writer.toString('a'));
    assertEquals("#\\b", writer.toString('b'));
    assertEquals("#\\Z", writer.toString('Z'));
  }

  @Test
  public void testWriteNil() {
    assertEquals(SCMNil.NIL.toString(), writer.toString(null));
    assertEquals("()", writer.toString(EMPTY));
    assertEquals("()", writer.toString(SCMCons.list()));
    assertEquals("()", writer.toString(Collections.EMPTY_LIST));
    assertEquals("()", writer.toString(Arrays.asList(1, 2, 3).subList(3, 3)));
  }

  @Test
  public void testWriteList() {
    assertEquals("(1 2 3)", writer.toString(Arrays.asList(1, 2, 3).subList(0, 3)));
    assertEquals("(1 2 3 4)", writer.toString(Arrays.asList(1, 2, 3, 4)));
    assertEquals("(#\\a #\\b #\\c)", writer.toString(Arrays.asList('a', 'b', 'c')));
    assertEquals("(\"test\" \"string\")", writer.toString(Arrays.asList("test", "string")));
    assertEquals(String.format("(%s %s %s)", SCMNil.NIL, SCMNil.NIL, SCMNil.NIL),
                 writer.toString(Arrays.asList(null, null, null)));
  }
}
