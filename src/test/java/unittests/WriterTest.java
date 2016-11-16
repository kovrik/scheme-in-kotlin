package unittests;

import core.scm.SCMCons;
import core.scm.SCMString;
import core.writer.IWriter;
import core.writer.Writer;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static core.scm.SCMCons.NIL;
import static org.junit.Assert.assertEquals;

public class WriterTest {

  private final IWriter writer = new Writer();

  @Test
  public void testWriteString() {
    assertEquals("\"test string\"", writer.toString(new SCMString("test string")));
    assertEquals("\"\"", writer.toString(new SCMString("")));
    assertEquals("\"(1 2 3)\"", writer.toString(new SCMString("(1 2 3)")));
  }

  @Test
  public void testWriteChar() {
    assertEquals("#\\a", writer.toString('a'));
    assertEquals("#\\b", writer.toString('b'));
    assertEquals("#\\Z", writer.toString('Z'));
  }

  @Test
  public void testWriteNil() {
    assertEquals("()", writer.toString(null));
    assertEquals("()", writer.toString(NIL));
    assertEquals("()", writer.toString(SCMCons.list()));
    assertEquals("()", writer.toString(new ArrayList()));
    assertEquals("()", writer.toString(Arrays.asList(1, 2, 3).subList(3, 3)));
  }

  @Test
  public void testWriteList() {
    assertEquals("(1 2 3)", writer.toString(Arrays.asList(1, 2, 3).subList(0, 3)));
    assertEquals("(1 2 3 4)", writer.toString(Arrays.asList(1, 2, 3, 4)));
    assertEquals("(#\\a #\\b #\\c)", writer.toString(Arrays.asList('a', 'b', 'c')));
    assertEquals("(\"test\" \"string\")", writer.toString(Arrays.asList(new SCMString("test"), new SCMString("string"))));
    assertEquals("(() () ())", writer.toString(Arrays.asList(null, null, null)));
  }
}
