package unittests;

import core.scm.SCMString;
import core.scm.SCMSymbol;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.list;
import static org.junit.Assert.*;

public class StringTest extends AbstractTest {

  @Test
  public void testEvalIsString() {
    assertEquals(FALSE, eval("(string? #\\A)", env));
    assertEquals(TRUE, eval("(string? \"A\")", env));
  }

  @Test
  public void testEvalStrings() {
    assertEquals(new SCMString("1"), eval("\"1\"", env));
    assertEquals(new SCMString("Lorem ipsum"), eval("\"Lorem ipsum\"", env));
    assertEquals(new SCMString("Lorem \\\"ipsum\\\" "), eval("\"Lorem \\\"ipsum\\\" \"", env));
    assertEquals(new SCMString(""), eval("\"\"", env));
  }

  @Test
  public void testEvalStringEq() {
    assertEquals(TRUE,  eval("(string=? \"test\" \"test\")", env));
    assertEquals(FALSE, eval("(string=? \"test\" \"test123\")", env));
    assertEquals(TRUE,  eval("(string=? \"\" \"\")", env));
    assertEquals(FALSE, eval("(string=? \"test\" \"Test\")", env));
  }

  @Test
  public void testEvalStringEqCi() {
    assertEquals(TRUE,  eval("(string-ci=? \"test\" \"test\")", env));
    assertEquals(FALSE, eval("(string-ci=? \"test\" \"test123\")", env));
    assertEquals(TRUE,  eval("(string-ci=? \"\" \"\")", env));
    assertEquals(TRUE,  eval("(string-ci=? \"test\" \"Test\")", env));
    assertEquals(TRUE,  eval("(string-ci=? \"tESt\" \"TesT\")", env));
  }

  @Test
  public void testEvalStringProc() {
    assertEquals(new SCMString(""), eval("(string)", env));
    assertEquals(new SCMString("a"), eval("(string #\\a)", env));
    assertEquals(new SCMString("abc"), eval("(string #\\a #\\b #\\c)", env));

    try {
      eval("(string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalMakeString() {
    assertEquals(new SCMString(""), eval("(make-string 0)", env));
    assertEquals(new SCMString(""), eval("(make-string 0 #\\a)", env));
    assertEquals(new SCMString("a"), eval("(make-string 1 #\\a)", env));
    assertEquals(new SCMString("aa"), eval("(make-string 2 #\\a)", env));
    assertEquals(new SCMString("ZZZZZZZZ"), eval("(make-string 8 #\\Z)", env));
    assertEquals(new SCMString("\u0000\u0000\u0000"), eval("(make-string 3)", env));
    assertEquals(new SCMString("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"), eval("(make-string 8)", env));

    try {
      eval("(make-string \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: \"test\""));
    }
    try {
      eval("(make-string 2 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Character, actual: 1"));
    }
    try {
      eval("(make-string)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong number of arguments (0) passed to: make-string"));
    }
    try {
      eval("(make-string 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong number of arguments (3) passed to: make-string"));
    }
  }

  @Test
  public void testEvalStringFill() {
    assertEquals(new SCMString(""), eval("(string-fill! \"\" #\\a)", env));
    assertEquals(new SCMString("a"), eval("(string-fill! \"z\" #\\a)", env));
    assertEquals(new SCMString("aaaaa"), eval("(string-fill! \"test1\" #\\a)", env));
  }

  @Test
  public void testEvalStringCopy() {
    assertEquals(new SCMString(""), eval("(string-copy \"\")", env));
    assertEquals(new SCMString("test"), eval("(string-copy \"test\")", env));
    assertEquals(new SCMString("t"), eval("(string-copy \"t\")", env));
  }

  @Test
  public void testEvalStringAppend() {
    assertEquals(new SCMString(""), eval("(string-append)", env));
    assertEquals(new SCMString(""), eval("(string-append \"\")", env));
    assertEquals(new SCMString("Apple"), eval("(string-append \"Apple\")", env));
    assertEquals(new SCMString("AppleBanana"), eval("(string-append \"Apple\" \"Banana\")", env));
    assertEquals(new SCMString("AppleBananaCoconut"), eval("(string-append \"Apple\" \"Banana\" \"Coconut\")", env));
  }

  @Test
  public void testEvalStringLength() {
    assertEquals(0L, eval("(string-length \"\")", env));
    assertEquals(0L, eval("(string-length (string))", env));
    assertEquals(1L, eval("(string-length \"1\")", env));
    assertEquals(3L, eval("(string-length \"123\")", env));

    try {
      eval("(string-length 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: String, actual: 1"));
    }
  }

  @Test
  public void testStringToList() {
    assertEquals(list('a', 'b', 'c'), eval("(string->list \"abc\")", env));
    assertEquals(list('a'), eval("(string->list \"a\")", env));
    assertEquals(list(), eval("(string->list \"\")", env));
    try {
      eval("(string->list (cons 1 2))", env);
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: String, actual: (1 . 2)", e.getMessage());
    }
  }

  @Test
  public void testEvalStringRef() {
    assertEquals('t', eval("(string-ref \"test string\" 0)", env));
    assertEquals('e', eval("(string-ref \"test string\" 1)", env));
    assertEquals('s', eval("(string-ref \"test string\" 2)", env));

    try {
      eval("(string-ref \"test\" -1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }
    try {
      eval("(string-ref \"tes\" 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }
    try {
      eval("(string-ref \"\" 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }
    try {
      eval("(string-ref '(1 2 3) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: String, actual: (1 2 3)"));
    }
    try {
      eval("(string-ref \"test\" 0.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
  }

  @Test
  public void testEvalStringSet() {
    assertEquals(new SCMString("z"),   eval("(let ((s \"a\"  )) (string-set! s 0 #\\z) s)", env));
    assertEquals(new SCMString("zbc"), eval("(let ((s \"abc\")) (string-set! s 0 #\\z) s)", env));
    assertEquals(new SCMString("azc"), eval("(let ((s \"abc\")) (string-set! s 1 #\\z) s)", env));
    assertEquals(new SCMString("abz"), eval("(let ((s \"abc\")) (string-set! s 2 #\\z) s)", env));

    try {
      eval("(string-set! \"abc\" -1 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: -1"));
    }
    try {
      eval("(string-set! \"abc\" 3 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 3"));
    }
    try {
      eval("(string-set! \"\" 0 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Value out of range: 0"));
    }
    try {
      eval("(string-set! '(1 2 3) 2 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: String, actual: (1 2 3)"));
    }
    try {
      eval("(string-set! \"test\" 0.5 #\\A)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Integer, actual: 0.5"));
    }
    try {
      eval("(string-set! \"test\" 3 '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().equals("Wrong argument type. Expected: Character, actual: ()"));
    }
  }

  @Test
  public void testEvalSymbolStringConversion() {
    assertEquals("test", eval("(symbol->string 'test)", env));
    assertEquals("test", eval("(symbol->string (string->symbol (symbol->string 'test)))", env));
    assertEquals(new SCMSymbol("test"), eval("(string->symbol (symbol->string 'test))", env));
    try {
      eval("(symbol->string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: Symbol, actual: 1"));
    }
    try {
      eval("(string->symbol 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage().contains("Wrong argument type. Expected: String, actual: 1"));
    }
  }
}
