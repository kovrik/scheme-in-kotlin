import core.parser.Tokenizer;
import core.scm.SCMList;
import core.scm.SCMVector;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TokenizerTest {

  private final Tokenizer tokenizer = new Tokenizer();

  @Test
  public void testParseNumbers() {

    assertEquals(1L, tokenizer.parse("1"));
    assertEquals(12345L, tokenizer.parse("12345"));
    assertEquals(-12345L, tokenizer.parse("-12345"));
    assertEquals(0L, tokenizer.parse("0"));

    assertEquals(0.5d, tokenizer.parse("0.5"));
    assertEquals(-0.5d, tokenizer.parse("-0.5"));
//    assertEquals(0.0d, tokenizer.parse("0.0"));
//    assertEquals(1235.0d, tokenizer.parse("1235.0"));
//    assertEquals(1235.0d, tokenizer.parse("1235."));
//    assertEquals(-1235.0d, tokenizer.parse("-1235."));
    assertEquals(.5d, tokenizer.parse(".5"));
    assertEquals(-.5d, tokenizer.parse("-.5"));
  }

  @Test
  public void testParseStrings() {

    assertEquals("1", tokenizer.parse("\"1\""));
    assertEquals("Lorem ipsum", tokenizer.parse("\"Lorem ipsum\""));
    assertEquals("Lorem \\\"ipsum\\\" ", tokenizer.parse("\"Lorem \\\"ipsum\\\" \""));
    assertEquals("", tokenizer.parse("\"\""));
  }

  @Test
  public void testParseVector() {

    assertEquals(new SCMVector(), tokenizer.parse("#()"));
    assertEquals(new SCMVector(0L), tokenizer.parse("#(0)"));
    assertEquals(new SCMVector(1L, 2L, 3L), tokenizer.parse("#(1 2 3)"));
    assertEquals(new SCMVector(1L, "test", 3L), tokenizer.parse("#(1 \"test\" 3)"));
    assertEquals(new SCMVector(1L, new SCMVector(2L), 3L), tokenizer.parse("#(1 #(2) 3)"));
  }

  @Test
  public void testParseList() {

    assertEquals(new SCMList(), tokenizer.parse("()"));
    assertEquals(new SCMList(0L), tokenizer.parse("(0)"));
    assertEquals(new SCMList(1L, 2L, 3L), tokenizer.parse("(1 2 3)"));
    assertEquals(new SCMList(1L, "test", 3L), tokenizer.parse("(1 \"test\" 3)"));
    assertEquals(new SCMList(1L, new SCMVector(2L), 3L), tokenizer.parse("(1 #(2) 3)"));
    assertEquals(new SCMList(1L, new SCMList<Object>(2L), 3L), tokenizer.parse("(1 (2) 3)"));
  }

  @Test
  public void testParseWhitespace() {

    assertEquals(null, tokenizer.parse(""));
    assertEquals(null, tokenizer.parse("\t"));
    assertEquals(null, tokenizer.parse("\n\r"));
  }
}
