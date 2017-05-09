package unittests;

import core.scm.Cons;
import core.writer.Writer;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static core.scm.Cons.EMPTY;
import static org.junit.Assert.assertEquals;

public class WriterTest {

  @Test
  public void testWriteString() {
    assertEquals("\"test string\"", Writer.write("test string"));
    assertEquals("\"\"", Writer.write(""));
    assertEquals("\"(1 2 3)\"", Writer.write("(1 2 3)"));
  }

  @Test
  public void testWriteChar() {
    assertEquals("#\\a", Writer.write('a'));
    assertEquals("#\\b", Writer.write('b'));
    assertEquals("#\\Z", Writer.write('Z'));
  }

  @Test
  public void testWriteNil() {
    assertEquals("nil", Writer.write((Object)null));
    assertEquals("()",  Writer.write(EMPTY));
    assertEquals("()",  Writer.write(Cons.list()));
    assertEquals("()",  Writer.write(Collections.EMPTY_LIST));
    assertEquals("()",  Writer.write(Arrays.asList(1, 2, 3).subList(3, 3)));
  }

  @Test
  public void testWriteList() {
    assertEquals("(1 2 3)", Writer.write(Arrays.asList(1, 2, 3).subList(0, 3)));
    assertEquals("(1 2 3 4)", Writer.write(Arrays.asList(1, 2, 3, 4)));
    assertEquals("(#\\a #\\b #\\c)", Writer.write(Arrays.asList('a', 'b', 'c')));
    assertEquals("(\"test\" \"string\")", Writer.write(Arrays.asList("test", "string")));
    assertEquals("(nil nil nil)", Writer.write(Arrays.asList(null, null, null)));
  }

  @Test
  public void testWriteEscape() {
    assertEquals("\"\\t\\b\\r\\n\\f\\\\\\\"\"", Writer.write("\t\b\r\n\f\\\""));;
  }
}
