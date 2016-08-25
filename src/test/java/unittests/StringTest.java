package unittests;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.reader.IReader;
import core.reader.Reader;
import core.scm.SCMSymbol;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.list;
import static org.junit.Assert.*;

public class StringTest {

  private final IReader reader = new Reader();
  private final IEvaluator eval = new Evaluator();
  private final DefaultEnvironment env = new DefaultEnvironment();
  {
    /* Eval lib procedures */
    for (String proc : env.getLibraryProcedures()) {
      eval(proc, env);
    }
  }

  /* Helper method */
  private Object eval(String sexp, IEnvironment env) {
    return eval.eval(reader.read(sexp), env);
  }

  @Test
  public void testEvalIsString() {
    assertEquals(FALSE, eval("(string? #\\A)", env));
    assertEquals(TRUE, eval("(string? \"A\")", env));
  }

  @Test
  public void testEvalStrings() {
    assertEquals("1", eval("\"1\"", env));
    assertEquals("Lorem ipsum", eval("\"Lorem ipsum\"", env));
    assertEquals("Lorem \"ipsum\" ", eval("\"Lorem \\\"ipsum\\\" \"", env));
    assertEquals("", eval("\"\"", env));
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
    assertEquals("", eval("(string)", env));
    assertEquals("a", eval("(string #\\a)", env));
    assertEquals("abc", eval("(string #\\a #\\b #\\c)", env));

    try {
      eval("(string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalMakeString() {
    assertEquals("", eval("(make-string 0)", env));
    assertEquals("", eval("(make-string 0 #\\a)", env));
    assertEquals("a", eval("(make-string 1 #\\a)", env));
    assertEquals("aa", eval("(make-string 2 #\\a)", env));
    assertEquals("ZZZZZZZZ", eval("(make-string 8 #\\Z)", env));

    assertEquals("\u0000\u0000\u0000", eval("(make-string 3)", env));
    assertEquals("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000", eval("(make-string 8)", env));

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
    assertEquals("", eval("(string-fill! \"\" #\\a)", env));
    assertEquals("a", eval("(string-fill! \"z\" #\\a)", env));
    assertEquals("aaaaa", eval("(string-fill! \"test1\" #\\a)", env));
  }

  @Test
  public void testEvalStringCopy() {
    assertEquals("", eval("(string-copy \"\")", env));
    assertEquals("test", eval("(string-copy \"test\")", env));
    assertEquals("t", eval("(string-copy \"t\")", env));
  }

  @Test
  public void testEvalStringAppend() {
    assertEquals("", eval("(string-append)", env));
    assertEquals("", eval("(string-append \"\")", env));
    assertEquals("Apple", eval("(string-append \"Apple\")", env));
    assertEquals("AppleBanana", eval("(string-append \"Apple\" \"Banana\")", env));
    assertEquals("AppleBananaCoconut", eval("(string-append \"Apple\" \"Banana\" \"Coconut\")", env));
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

  // FIXME Make modifiable String
  @Test
  public void testEvalStringSet() {
    assertEquals("z", eval("(string-set! \"a\" 0 #\\z)", env));
    assertEquals("zbc", eval("(string-set! \"abc\" 0 #\\z)", env));
    assertEquals("azc", eval("(string-set! \"abc\" 1 #\\z)", env));
    assertEquals("abz", eval("(string-set! \"abc\" 2 #\\z)", env));

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
