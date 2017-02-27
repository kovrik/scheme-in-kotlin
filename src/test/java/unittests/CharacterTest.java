package unittests;

import core.exceptions.WrongTypeException;
import org.junit.Test;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class CharacterTest extends AbstractTest {

  @Test
  public void testEvalIsChar() {
    assertEquals(TRUE, eval("(char? #\\A)", env));
    assertEquals(TRUE, eval("(char? #\\u0000)", env));
    assertEquals(TRUE, eval("(char? #\\u0300)", env));
    assertEquals(FALSE, eval("(char? \"A\")", env));
  }

  @Test
  public void testEvalCharEq() {
    assertEquals(TRUE,  eval("(char=? #\\A #\\A)", env));
    assertEquals(FALSE, eval("(char=? #\\B #\\A)", env));
    assertEquals(TRUE,  eval("(char=? #\\newline #\\newline)", env));
  }

  @Test
  public void testEvalCharEqCi() {
    assertEquals(TRUE,  eval("(char-ci=? #\\Z #\\z)", env));
    assertEquals(FALSE, eval("(char-ci=? #\\b #\\A)", env));
  }

  @Test
  public void testEvalCharNumeric() {
    assertEquals(TRUE,  eval("(char-numeric? #\\1)", env));
    assertEquals(TRUE,  eval("(char-numeric? #\\0)", env));
    assertEquals(TRUE,  eval("(char-numeric? #\\9)", env));
    assertEquals(FALSE, eval("(char-numeric? #\\b)", env));
    assertEquals(FALSE, eval("(char-numeric? #\\.)", env));
    try {
      eval("(char-numeric? 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char-numeric?: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalCharWhitespace() {
    assertEquals(FALSE, eval("(char-whitespace? #\\1)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\0)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\9)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\b)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\.)", env));
    assertEquals(FALSE, eval("(char-whitespace? #\\backspace)", env));

    assertEquals(TRUE, eval("(char-whitespace? #\\newline)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\tab)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\vtab)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\return)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\space)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\page)", env));
    assertEquals(TRUE, eval("(char-whitespace? #\\linefeed)", env));
    try {
      eval("(char-whitespace? 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char-whitespace?: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalCharAlphabetic() {
    assertEquals(FALSE, eval("(char-alphabetic? #\\1)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\0)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\9)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\.)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\backspace)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\newline)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\tab)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\vtab)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\return)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\space)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\page)", env));
    assertEquals(FALSE, eval("(char-alphabetic? #\\linefeed)", env));

    assertEquals(TRUE, eval("(char-alphabetic? #\\b)", env));
    assertEquals(TRUE, eval("(char-alphabetic? #\\Z)", env));
    assertEquals(TRUE, eval("(char-alphabetic? #\\g)", env));
    assertEquals(TRUE, eval("(char-alphabetic? #\\I)", env));
    try {
      eval("(char-alphabetic? 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char-alphabetic?: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalCharUpperCase() {
    assertEquals(FALSE, eval("(char-upper-case? #\\1)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\0)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\9)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\b)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\.)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\a)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\z)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\i)", env));
    assertEquals(FALSE, eval("(char-upper-case? #\\h)", env));

    assertEquals(TRUE, eval("(char-upper-case? #\\A)", env));
    assertEquals(TRUE, eval("(char-upper-case? #\\Z)", env));
    assertEquals(TRUE, eval("(char-upper-case? #\\I)", env));
    assertEquals(TRUE, eval("(char-upper-case? #\\H)", env));
    try {
      eval("(char-upper-case? 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char-upper-case?: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalCharLowerCase() {
    assertEquals(TRUE, eval("(char-lower-case? #\\b)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\a)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\z)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\i)", env));
    assertEquals(TRUE, eval("(char-lower-case? #\\h)", env));

    assertEquals(FALSE, eval("(char-lower-case? #\\A)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\Z)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\I)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\H)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\1)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\0)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\9)", env));
    assertEquals(FALSE, eval("(char-lower-case? #\\.)", env));
    try {
      eval("(char-lower-case? 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char-lower-case?: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalCharToInteger() {
    assertEquals(49L, eval("(char->integer #\\1)", env));
    assertEquals(48L, eval("(char->integer #\\0)", env));
    assertEquals(57L, eval("(char->integer #\\9)", env));
    assertEquals(98L, eval("(char->integer #\\b)", env));
    assertEquals(46L, eval("(char->integer #\\.)", env));
    try {
      eval("(char->integer 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char->integer: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalIntegerToChar() {
    assertEquals('1', eval("(integer->char (char->integer #\\1))", env));
    assertEquals('0', eval("(integer->char (char->integer #\\0))", env));
    assertEquals('9', eval("(integer->char (char->integer #\\9))", env));
    assertEquals('b', eval("(integer->char (char->integer #\\b))", env));
    assertEquals('.', eval("(integer->char (char->integer #\\.))", env));
    try {
      eval("(integer->char #\\a)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("integer->char: type mismatch; (expected: Integer, given: #\\a)", e.getMessage());
    }
  }

  @Test
  public void testEvalCharUpcase() {
    assertEquals('1', eval("(char-upcase #\\1)", env));
    assertEquals('0', eval("(char-upcase #\\0)", env));
    assertEquals('9', eval("(char-upcase #\\9)", env));
    assertEquals('B', eval("(char-upcase #\\b)", env));
    assertEquals('Z', eval("(char-upcase #\\z)", env));
    assertEquals('.', eval("(char-upcase #\\.)", env));
    try {
      eval("(char-upcase 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char-upcase: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalCharDowncase() {
    assertEquals('1', eval("(char-downcase #\\1)", env));
    assertEquals('0', eval("(char-downcase #\\0)", env));
    assertEquals('9', eval("(char-downcase #\\9)", env));
    assertEquals('b', eval("(char-downcase #\\B)", env));
    assertEquals('z', eval("(char-downcase #\\Z)", env));
    assertEquals('.', eval("(char-downcase #\\.)", env));
    try {
      eval("(char-downcase 1)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("char-downcase: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testCharComparison() {
    assertEquals(TRUE,  eval("(char<? #\\a #\\b)", env));
    assertEquals(TRUE,  eval("(char<=? #\\a #\\a #\\b)", env));
    assertEquals(FALSE, eval("(char<=? #\\a #\\A #\\b)", env));
    assertEquals(FALSE, eval("(char>? #\\z #\\A #\\H #\\a #\\X)", env));
    assertEquals(TRUE, eval("(char>=? #\\z #\\z #\\x)", env));
  }
}
