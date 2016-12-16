package unittests;

import core.exceptions.IllegalSyntaxException;
import core.reader.StringReader;
import core.scm.*;
import core.scm.specialforms.Quasiquote;
import core.scm.specialforms.Quote;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static core.scm.SCMCons.list;
import static core.scm.specialforms.Lambda.LAMBDA;
import static org.junit.Assert.*;

public class ReaderTest {

  private final StringReader reader = new StringReader();

  @Test
  public void testReadNumbers() {
    assertEquals(1L, reader.readFirst("1"));
    assertEquals(12345L, reader.readFirst("12345"));
    assertEquals(-12345L, reader.readFirst("-12345"));
    assertEquals(0L, reader.readFirst("0"));

    assertEquals(0.5d, reader.readFirst("0.5"));
    assertEquals(-0.5d, reader.readFirst("-0.5"));
    assertEquals(0.0d, reader.readFirst("0.0"));
    assertEquals(1235.0d, reader.readFirst("1235.0"));
    assertEquals(1235.0d, reader.readFirst("1235."));
    assertEquals(-1235.0d, reader.readFirst("-1235."));
    assertEquals(.5d, reader.readFirst(".5"));
    assertEquals(-.5d, reader.readFirst("-.5"));

    assertEquals(new SCMBigRational("-1", "2"), reader.readFirst("#e#d-.5"));
    assertEquals(new SCMBigRational("-1", "2"), reader.readFirst("#E#d-.5"));
    assertEquals(+4.5d, reader.readFirst("#i#d+4.5"));
    assertEquals(4999999.5d, reader.readFirst("#i#d+4999999.5"));
    assertEquals(5L, reader.readFirst("#e#b101"));
    assertEquals(5L, reader.readFirst("#b#e101"));
    assertEquals(1L, reader.readFirst("#b#e0001"));
    assertEquals(5L, reader.readFirst("#E#b101"));
    assertEquals(5L, reader.readFirst("#b#E101"));
    assertEquals(1L, reader.readFirst("#b#E0001"));
    assertEquals(455L, reader.readFirst("#o0707"));
    assertEquals(585L, reader.readFirst("#o1111"));
    assertEquals(new BigDecimal("324518553658426726783156020576255"), reader.readFirst("#xfffffffffffffffffffffffffff"));
    try {
      reader.readFirst("#o9999");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad number: #o9999", e.getMessage());
    }
    try {
      reader.readFirst("#df999");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad number: #df999", e.getMessage());
    }
    try {
      reader.readFirst("#xz999");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad number: #xz999", e.getMessage());
    }
    try {
      reader.readFirst("#b2222");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad number: #b2222", e.getMessage());
    }
    try {
      reader.readFirst("#d+5+5");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad number: #d+5+5", e.getMessage());
    }
    try {
      reader.readFirst("+5+5");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad number: +5+5", e.getMessage());
    }
    assertEquals(255.99609375, reader.readFirst("#d255.99609375"));
    assertEquals(255.99609375, reader.readFirst("#xff.ff"));
    assertEquals(171.67111108726925, reader.readFirst("#xab.abcdefabcdef"));
    assertEquals(3.3125, reader.readFirst("#b11.0101"));
    assertEquals(2730.661460876465, reader.readFirst("#b101010101010.10101001010101011"));
    assertEquals(83.97128295898438, reader.readFirst("#o123.76123"));
    assertEquals(2054353.1632647514, reader.readFirst("#o7654321.1234567"));
    assertEquals(1500d, reader.readFirst("15##"));
    assertEquals(1500d, reader.readFirst("15##."));
    assertEquals(1500d, reader.readFirst("15##.#"));
    assertEquals(1500d, reader.readFirst("15##.####"));
    assertEquals(1500d, reader.readFirst("#i15##.####"));
    assertEquals(1500d, reader.readFirst("#i15##"));
    assertEquals(1500d, reader.readFirst("#I15##.####"));
    assertEquals(1500d, reader.readFirst("#I15##"));
    assertEquals(new SCMBigRational("500", BigInteger.ONE), reader.readFirst("#e5###/1#"));
    assertEquals(new SCMBigRational("500", BigInteger.ONE), reader.readFirst("#E5###/1#"));
    assertEquals(new BigDecimal("500.0"), reader.readFirst(" 5###/1#"));
    assertEquals(new SCMBigRational("1500", BigInteger.ONE), reader.readFirst("#e15##.####"));
    assertEquals(new BigDecimal("0.75"), reader.readFirst("#i3/4"));
    assertEquals(new SCMBigRational("3", "4"), reader.readFirst("#e3/4"));
  }

  @Test
  public void testReadComplex() {
    assertEquals(new SCMBigComplex(new BigDecimal(1),  new BigDecimal(2)),  reader.readFirst("1+2i"));
    assertEquals(new SCMBigComplex(new BigDecimal(1),  new BigDecimal(-2)), reader.readFirst("1-2i"));
    assertEquals(new SCMBigComplex(new BigDecimal(-1), new BigDecimal(2)),  reader.readFirst("-1+2i"));
    assertEquals(new SCMBigComplex(new BigDecimal(-1), new BigDecimal(-2)), reader.readFirst("-1-2i"));
    assertEquals(new SCMBigComplex(new BigDecimal(1),  new BigDecimal(2)),  reader.readFirst("#e1+2i"));
    assertEquals(new SCMBigComplex(new BigDecimal("1.0"), new BigDecimal("2.0")),  reader.readFirst("#i1+2i"));
    assertEquals(new SCMBigComplex(new BigDecimal("5"),   new BigDecimal("29")),  reader.readFirst("#e#b101+11101i"));
    assertEquals(new SCMBigComplex(new BigDecimal("5"),   new BigDecimal("29")),  reader.readFirst("#e#b101+11101i"));
    assertEquals(new SCMBigComplex(new BigDecimal("255.0"),  new BigDecimal("2987.9375")), reader.readFirst("#x#iFf+BaB.fI"));
    // TODO #e3/4+2.5i
  }

  @Test
  public void testReadStrings() {
    assertEquals("1", reader.readFirst("\"1\""));
    assertEquals("Lorem ipsum", reader.readFirst("\"Lorem ipsum\""));
    assertEquals("Lorem \\\"ipsum\\\" ", reader.readFirst("\"Lorem \\\"ipsum\\\" \""));
    assertEquals("", reader.readFirst("\"\""));
    assertEquals("test \u0123", reader.readFirst("\"test \\u123\""));
    assertEquals("test \\\\u", reader.readFirst("\"test \\\\u\""));
    assertEquals("test \\\\U", reader.readFirst("\"test \\\\U\""));
    assertEquals("test \\\\x", reader.readFirst("\"test \\\\x\""));
    try {
      reader.readFirst("\"test \\uu\"");
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: no hex digit following \\u in string", e.getMessage());
    }
    try {
      reader.readFirst("\"test \\u\"");
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: no hex digit following \\u in string", e.getMessage());
    }
  }

  @Test
  public void testReadVector() {
    assertEquals(new SCMMutableVector(), reader.readFirst("#()"));
    assertEquals(new SCMMutableVector(0L), reader.readFirst("#(0)"));
    assertEquals(new SCMMutableVector(1L, 2L, 3L), reader.readFirst("#(1 2 3)"));
    assertEquals(new SCMMutableVector(1L, "test", 3L), reader.readFirst("#(1 \"test\" 3)"));
    assertEquals(new SCMMutableVector(1L, new SCMMutableVector(2L), 3L), reader.readFirst("#(1 #(2) 3)"));
    try {
      reader.readFirst("#(1 . 2)");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: illegal use of '.'", e.getMessage());
    }
    try {
      reader.readFirst("#(1 2 3 4 5 . 6)");
    } catch (IllegalSyntaxException e) {
      assertEquals("read: illegal use of '.'", e.getMessage());
    }
  }

  @Test
  public void testReadList() {
    assertEquals(list(), reader.readFirst("()"));
    assertEquals(list(0L), reader.readFirst("(0)"));
    assertEquals(list(1L, 2L, 3L), reader.readFirst("(1 2 3)"));
    assertEquals(list(1L, "test", 3L), reader.readFirst("(1 \"test\" 3)"));
    assertEquals(list(1L, new SCMMutableVector(2L), 3L), reader.readFirst("(1 #(2) 3)"));
    assertEquals(list(1L, list(2L), 3L), reader.readFirst("(1 (2) 3)"));
  }

  @Test
  public void testReadWhitespace() {
    assertEquals(null, reader.readFirst(""));
    assertEquals(null, reader.readFirst("\t"));
    assertEquals(null, reader.readFirst("\n\r"));
    assertEquals(list(s("+"), 1L, 2L, 3L), reader.readFirst("(+ 1 2 \t \n    \f \r 3\t\n\r\f)"));
  }

  @Test
  public void testReadQuote() {
    assertEquals(list(s(Quote.QUOTE.toString()), 1L), reader.readFirst("'1"));
    assertEquals(list(s(Quote.QUOTE.toString()), list(1L, "test")), reader.readFirst("'(1 \"test\")"));
    assertEquals(list(s(Quote.QUOTE.toString()), list(s(Quote.QUOTE.toString()), 1L)), reader.readFirst("''1"));
  }

  @Test
  public void testReadQuasiquote() {
    assertEquals(list(s(Quasiquote.QUASIQUOTE.toString()), 1L), reader.readFirst("`1"));
    assertEquals(list(s(Quasiquote.QUASIQUOTE.toString()), list(1L, "test")), reader.readFirst("`(1 \"test\")"));
    assertEquals(list(s(Quasiquote.QUASIQUOTE.toString()), list(s(Quote.QUOTE.toString()), 1L)), reader.readFirst("`'1"));
  }

  @Test
  public void testReadComment() {
    assertEquals(null, reader.readFirst(";test"));
    assertEquals(null, reader.readFirst(";test\ttest"));
    assertEquals(null, reader.readFirst(";test\t\ntest"));
    assertEquals(reader.readFirst("1"), reader.readFirst("1; wefewffewfwfwe \t \t few fwe f wf wfw ;w effw efw e "));
    assertEquals(reader.readFirst("1"), reader.readFirst("1 ; test"));
    assertEquals(reader.readFirst("'(1 \"a\" 5)"), reader.readFirst("'(1 \"a\" 5) ; test"));
  }

  @Test
  public void testReadCharacter() {
    assertEquals('A', reader.readFirst("#\\A"));
    assertEquals('z', reader.readFirst("#\\z"));
    assertEquals('5', reader.readFirst("#\\5"));
    assertEquals(' ', reader.readFirst("#\\space"));
    assertEquals('\n', reader.readFirst("#\\newline"));
    assertEquals('\u000B', reader.readFirst("#\\13"));
    assertEquals('\u0009', reader.readFirst("#\\11"));
    assertEquals('0', reader.readFirst("#\\0"));
    assertEquals('8', reader.readFirst("#\\8"));
    assertEquals('3', reader.readFirst("#\\3"));
    assertEquals('\u0007', reader.readFirst("#\\alarm"));
    assertEquals('\u0000', reader.readFirst("#\\nul"));
    assertEquals('\u0000', reader.readFirst("#\\null"));
    assertEquals(Character.MIN_VALUE, reader.readFirst("#\\null"));
    assertEquals(Character.MIN_VALUE, reader.readFirst("#\\u00000"));
    assertEquals(Character.MIN_VALUE, reader.readFirst("#\\u0"));
    assertEquals('\r', reader.readFirst("#\\ud"));
    assertEquals('\u000b', reader.readFirst("#\\ub"));
    assertEquals('\u000b', reader.readFirst("#\\u000b"));
    assertEquals('\u000b', reader.readFirst("#\\u000000b"));
    assertEquals('u', reader.readFirst("#\\u"));
    assertEquals('U', reader.readFirst("#\\U"));
    assertEquals('x', reader.readFirst("#\\x"));
    assertEquals('X', reader.readFirst("#\\X"));
  }

  @Test
  public void testReadBoolean() {
    assertEquals(SCMBoolean.TRUE,  reader.readFirst("#t"));
    assertEquals(SCMBoolean.FALSE, reader.readFirst("#f"));
    assertEquals(SCMBoolean.TRUE,  reader.readFirst("#T"));
    assertEquals(SCMBoolean.FALSE, reader.readFirst("#F"));
  }

  @Test
  public void testReadIdentifier() {
    assertEquals(new SCMSymbol("test"), reader.readFirst("test"));
    assertEquals(new SCMSymbol(LAMBDA.toString()), reader.readFirst("lambda"));
    assertEquals(new SCMSymbol("list->vector"), reader.readFirst("list->vector"));
    assertEquals(new SCMSymbol("+"), reader.readFirst("+"));
    assertEquals(new SCMSymbol("<=?"), reader.readFirst("<=?"));
    assertEquals(new SCMSymbol("the-word-recursion-has-many-meanings"), reader.readFirst("the-word-recursion-has-many-meanings"));
    assertEquals(new SCMSymbol("a34kTMNs"), reader.readFirst("a34kTMNs"));
    assertEquals(new SCMSymbol("V17a"), reader.readFirst("V17a"));
    assertEquals(new SCMSymbol("soup"), reader.readFirst("soup"));
    assertEquals(new SCMSymbol("a"), reader.readFirst("a"));
    assertEquals(new SCMSymbol("ab"), reader.readFirst("ab"));
  }


  @Test
  public void testScientificNotation() {
    assertEquals(new SCMBigRational("23", "1"), reader.readFirst("#e2.3e1"));
    assertEquals(230L, reader.readFirst("#e23e1"));
    assertEquals(Double.valueOf("2.3e-5"), reader.readFirst("#i2.3e-5"));
    assertEquals(new BigDecimal("2.3e-51"), reader.readFirst("#i2.3e-51"));
    assertEquals(Double.valueOf("2.3e-5"), reader.readFirst("#I2.3e-5"));
    assertEquals(new BigDecimal("2.3e-51"), reader.readFirst("#I2.3e-51"));
    assertEquals(new BigDecimal("92160.0"), reader.readFirst("#b101101e1011"));
    assertEquals(new BigDecimal("4484907929698304.0"), reader.readFirst("#xfefsa"));
  }

  @Test
  public void testEscapeSequences() {
    String escape   = "\"\\a\\b\\t\\n\\v\\e\\f\\r\\\"\\\'\\\\\"";
    String expected = "\\a\\b\\t\\n\\v\\e\\f\\r\\\"\\\'\\\\";
    assertEquals(expected, reader.readFirst(escape));
    try {
      reader.readFirst("\"\\u\"");
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: no hex digit following \\u in string", e.getMessage());
    }
    try {
      reader.readFirst("\"\\x\"");
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: unknown escape sequence \\x in string", e.getMessage());
    }
  }

  private static SCMSymbol s(String str) {
    return new SCMSymbol(str);
  }
}
