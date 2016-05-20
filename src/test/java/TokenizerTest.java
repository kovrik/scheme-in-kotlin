import core.parser.Tokenizer;
import core.scm.SCMBoolean;
import core.scm.SCMList;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.SCMSpecialForm;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TokenizerTest {

  private final Tokenizer tokenizer = new Tokenizer();

  @Test
  public void testReadNumbers() {

    assertEquals(1L, tokenizer.parse("1"));
    assertEquals(12345L, tokenizer.parse("12345"));
    assertEquals(-12345L, tokenizer.parse("-12345"));
    assertEquals(0L, tokenizer.parse("0"));

    assertEquals(0.5d, tokenizer.parse("0.5"));
    assertEquals(-0.5d, tokenizer.parse("-0.5"));
    assertEquals(0.0d, tokenizer.parse("0.0"));
    assertEquals(1235.0d, tokenizer.parse("1235.0"));
    assertEquals(1235.0d, tokenizer.parse("1235."));
    assertEquals(-1235.0d, tokenizer.parse("-1235."));
    assertEquals(.5d, tokenizer.parse(".5"));
    assertEquals(-.5d, tokenizer.parse("-.5"));
  }

  @Test
  public void testReadStrings() {

    assertEquals("1", tokenizer.parse("\"1\""));
    assertEquals("Lorem ipsum", tokenizer.parse("\"Lorem ipsum\""));
    assertEquals("Lorem \\\"ipsum\\\" ", tokenizer.parse("\"Lorem \\\"ipsum\\\" \""));
    assertEquals("", tokenizer.parse("\"\""));
  }

  @Test
  public void testReadVector() {

    assertEquals(new SCMVector(), tokenizer.parse("#()"));
    assertEquals(new SCMVector(0L), tokenizer.parse("#(0)"));
    assertEquals(new SCMVector(1L, 2L, 3L), tokenizer.parse("#(1 2 3)"));
    assertEquals(new SCMVector(1L, "test", 3L), tokenizer.parse("#(1 \"test\" 3)"));
    assertEquals(new SCMVector(1L, new SCMVector(2L), 3L), tokenizer.parse("#(1 #(2) 3)"));
  }

  @Test
  public void testReadList() {

    assertEquals(new SCMList(), tokenizer.parse("()"));
    assertEquals(new SCMList(0L), tokenizer.parse("(0)"));
    assertEquals(new SCMList(1L, 2L, 3L), tokenizer.parse("(1 2 3)"));
    assertEquals(new SCMList(1L, "test", 3L), tokenizer.parse("(1 \"test\" 3)"));
    assertEquals(new SCMList(1L, new SCMVector(2L), 3L), tokenizer.parse("(1 #(2) 3)"));
    assertEquals(new SCMList(1L, new SCMList<Object>(2L), 3L), tokenizer.parse("(1 (2) 3)"));
  }

  @Test
  public void testReadWhitespace() {

    assertEquals(null, tokenizer.parse(""));
    assertEquals(null, tokenizer.parse("\t"));
    assertEquals(null, tokenizer.parse("\n\r"));
  }

  @Test
  public void testReadQuote() {

    assertEquals(new SCMList<Object>(SCMSpecialForm.QUOTE, 1L), tokenizer.parse("'1"));
    assertEquals(new SCMList<Object>(SCMSpecialForm.QUOTE, new SCMList<Object>(1L, "test")), tokenizer.parse("'(1 \"test\")"));
    assertEquals(new SCMList<Object>(SCMSpecialForm.QUOTE, new SCMList<Object>(SCMSpecialForm.QUOTE, 1L)), tokenizer.parse("''1"));
  }

  @Test
  public void testReadComment() {

    assertEquals(null, tokenizer.parse(";test"));
    assertEquals(tokenizer.parse("1"), tokenizer.parse("1 ; test"));
    assertEquals(tokenizer.parse("'(1 \"a\" 5)"), tokenizer.parse("'(1 \"a\" 5) ; test"));
  }

  @Test
  public void testReadCharacter() {

    assertEquals('A', tokenizer.parse("#\\A"));
    assertEquals('z', tokenizer.parse("#\\z"));
    assertEquals('5', tokenizer.parse("#\\5"));
    assertEquals(' ', tokenizer.parse("#\\space"));
    assertEquals('\n', tokenizer.parse("#\\newline"));
  }

  @Test
  public void testReadBoolean() {

    assertEquals(SCMBoolean.TRUE, tokenizer.parse("#t"));
    assertEquals(SCMBoolean.FALSE, tokenizer.parse("#f"));
  }

  @Test
  public void testReadIdentifier() {

    assertEquals(new SCMSymbol("test"), tokenizer.parse("test"));
    assertEquals(SCMSpecialForm.LAMBDA, tokenizer.parse("lambda"));
    assertEquals(new SCMSymbol("list->vector"), tokenizer.parse("list->vector"));
    assertEquals(new SCMSymbol("+"), tokenizer.parse("+"));
    assertEquals(new SCMSymbol("<=?"), tokenizer.parse("<=?"));
    assertEquals(new SCMSymbol("the-word-recursion-has-many-meanings"), tokenizer.parse("the-word-recursion-has-many-meanings"));
    assertEquals(new SCMSymbol("a34kTMNs"), tokenizer.parse("a34kTMNs"));
    assertEquals(new SCMSymbol("V17a"), tokenizer.parse("V17a"));
    assertEquals(new SCMSymbol("soup"), tokenizer.parse("soup"));
    assertEquals(new SCMSymbol("a"), tokenizer.parse("a"));
    assertEquals(new SCMSymbol("ab"), tokenizer.parse("ab"));
  }
}
