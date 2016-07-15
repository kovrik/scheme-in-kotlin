package s7.tests;

import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static junit.framework.TestCase.assertEquals;

public class IsCharTest extends AbstractS7Test {

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

//    assertEquals(, eval("(char?) 'error)
//    assertEquals(, eval("(char? #\a #\b) 'error)
//    assertEquals(, eval("(char #\a) 'error)

    // TODO
//    assertEquals(TRUE,  eval("(char? #\\x65)", env));
//    assertEquals(TRUE,  eval("(char? #\\x000000000065)", env));
//    assertEquals(TRUE,  eval("(char? #\\x0)", env));
//    assertEquals(TRUE,  eval("(char=? #\\x000 #\\null)", env));
//    assertEquals(TRUE,  eval("(char=? #\\x08 #\\x8)", env));
//    ; Guile thinks both of these names are bogus
//    assertEquals(TRUE,  eval("(char=? #\\x0e    #\\xe)", env));
//    assertEquals(TRUE,  eval("(char=? #\\x00e   #\\xe)", env));
//    assertEquals(TRUE,  eval("(char=? #\\x0000e #\\xe)", env));
//    ; hmmm -- surely this is a bug
//    assertEquals(TRUE,  eval("(char=? #\\x00000000e #\\xe)", env));
//    assertEquals(TRUE,  eval("(char? #\\xff)", env));
//    assertEquals(TRUE,  eval("(char=? #\\x6a #\\j)", env));
//    assertEquals(TRUE,  eval("(char=? #\\return #\\xd)", env));
//    assertEquals(TRUE,  eval("(char=? #\\null #\\x0)", env));

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

//    assertEquals(TRUE,  eval("(apply char? (list (integer->char 255)))", env));
//    ;(test (char? #\ÿ) #t) ; this seems to involve unwanted translations in emacs?
//    assertEquals(FALSE, eval("(char? #<eof>)", env));
//    assertEquals(FALSE, eval("(char? '1e311)", env));
  }
}
