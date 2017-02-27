package unittests.s7.tests;

import core.exceptions.ArityException;
import org.junit.Test;
import unittests.AbstractTest;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.fail;

public class IsCharTest extends AbstractTest {

  @Test
  public void testIsChar() {
    assertEquals(TRUE,  eval("(char? #\\a)", env));
    assertEquals(TRUE,  eval("(char? #\\()", env));
    assertEquals(TRUE,  eval("(char? #\\space)", env));
    assertEquals(TRUE,  eval("(char? '#\\newline)", env));
    assertEquals(TRUE,  eval("(char? #\\1)", env));
    assertEquals(TRUE,  eval("(char? #\\$)", env));
    assertEquals(TRUE,  eval("(char? #\\.)", env));
    assertEquals(TRUE,  eval("(char? #\\\\)", env));
    assertEquals(TRUE,  eval("(char? #\\))", env));
    assertEquals(TRUE,  eval("(char? #\\%)", env));
    assertEquals(TRUE,  eval("(char? '#\\space)", env));
    assertEquals(TRUE,  eval("(char? '#\\ )", env));
    assertEquals(TRUE,  eval("(char? '#\\newline)", env));
    assertEquals(TRUE,  eval("(char? '#\\a)", env));
    assertEquals(TRUE,  eval("(char? '#\\8)", env));
    assertEquals(TRUE,  eval("(char? #\\-)", env));
    assertEquals(TRUE,  eval("(char? #\\n)", env));
    assertEquals(TRUE,  eval("(char? #\\()", env));
    assertEquals(FALSE, eval("(char? #e1)", env));
    assertEquals(TRUE,  eval("(char? #\\#)", env));
    assertEquals(TRUE,  eval("(char? #\\x)", env));
    assertEquals(TRUE,  eval("(char? #\\o)", env));
    assertEquals(TRUE,  eval("(char? #\\b)", env));
    assertEquals(FALSE, eval("(char? #b101)", env));
    assertEquals(FALSE, eval("(char? #o73)", env));
    assertEquals(FALSE, eval("(char? #x73)", env));
    assertEquals(FALSE, eval("(char? 'a)", env));
    assertEquals(FALSE, eval("(char? 97)", env));
    assertEquals(FALSE, eval("(char? \"a\")", env));
    assertEquals(TRUE,  eval("(char? (string-ref \"hi\" 0))", env));
    assertEquals(TRUE,  eval("(char? (string-ref (make-string 1) 0))", env));
    assertEquals(TRUE,  eval("(char? #\\\")", env));
    assertEquals(TRUE,  eval("(char? #\\\')", env));
    assertEquals(TRUE,  eval("(char? #\\`)", env));
    assertEquals(TRUE,  eval("(char? #\\@)", env));
    assertEquals(FALSE, eval("(char? 'begin)", env));
    assertEquals(TRUE,  eval("(char? #\\u65)", env));
    assertEquals(TRUE,  eval("(char? #\\u000000000065)", env));
    assertEquals(TRUE,  eval("(char? #\\u0)", env));
    assertEquals(TRUE,  eval("(char=? #\\u000 #\\null)", env));
    assertEquals(TRUE,  eval("(char=? #\\u08 #\\u8)", env));
//    ; Guile thinks both of these names are bogus
    assertEquals(TRUE,  eval("(char=? #\\u0e    #\\ue)", env));
    assertEquals(TRUE,  eval("(char=? #\\u00e   #\\ue)", env));
    assertEquals(TRUE,  eval("(char=? #\\u0000e #\\ue)", env));
//    ; hmmm -- surely this is a bug
    assertEquals(TRUE,  eval("(char=? #\\u00000000e #\\ue)", env));
    assertEquals(TRUE,  eval("(char? #\\uff)", env));
    assertEquals(TRUE,  eval("(char=? #\\u6a #\\j)", env));
    assertEquals(TRUE,  eval("(char=? #\\return #\\ud)", env));
    assertEquals(TRUE,  eval("(char=? #\\null #\\u0)", env));
    assertEquals(TRUE,  eval("(char? #\\return)", env));
    assertEquals(TRUE,  eval("(char? #\\null)", env));
    assertEquals(TRUE,  eval("(char? #\\nul)", env));
    assertEquals(TRUE,  eval("(char? #\\linefeed)", env));
    assertEquals(TRUE,  eval("(char? #\\tab)", env));
    assertEquals(TRUE,  eval("(char? #\\space)", env));
    assertEquals(TRUE,  eval("(char=? #\\null #\\nul)", env));
    assertEquals(TRUE,  eval("(char=? #\\newline #\\linefeed)", env));
    assertEquals(TRUE,  eval("(char? #\\backspace)", env));
    assertEquals(TRUE,  eval("(char? #\\page)", env));
    assertEquals(TRUE,  eval("(char? #\\escape)", env));
    assertEquals(TRUE,  eval("(char? #\\alarm)", env));
    assertEquals(TRUE,  eval("(char? #\\delete)", env));
    assertEquals(FALSE, eval("(char=? #\\delete #\\backspace)", env));
    assertEquals(FALSE, eval("(char? '1e311)", env));
    try {
      eval("(char?) 'error)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("char?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.getMessage());
    }
    try {
      eval("(char? #\\a #\\b)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("char?: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 2)", e.getMessage());
    }
//    assertEquals(TRUE,  eval("(apply char? (list (integer->char 255)))", env));
//    ;(test (char? #\ÿ) #t) ; this seems to involve unwanted translations in emacs?
//    assertEquals(FALSE, eval("(char? #<eof>)", env));
  }
}
