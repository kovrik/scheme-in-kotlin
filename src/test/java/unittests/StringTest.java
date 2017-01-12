package unittests;

import core.exceptions.WrongTypeException;
import core.scm.SCMClass;
import core.scm.SCMMutableString;
import core.scm.SCMSymbol;
import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static core.scm.SCMCons.list;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class StringTest extends AbstractTest {

  @Test
  public void testEvalIsString() {
    assertEquals(FALSE, eval("(string? #\\A)", env));
    assertEquals(TRUE, eval("(string? \"A\")", env));
  }

  @Test
  public void testEvalStrings() {
    assertEquals("1", eval("\"1\"", env));
    assertEquals("Lorem ipsum", eval("\"Lorem ipsum\"", env));
    assertEquals("Lorem \\\"ipsum\\\" ", eval("\"Lorem \\\"ipsum\\\" \"", env));
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
    assertEquals(new SCMMutableString(""), eval("(string)", env));
    assertEquals(new SCMMutableString("a"), eval("(string #\\a)", env));
    assertEquals(new SCMMutableString("abc"), eval("(string #\\a #\\b #\\c)", env));

    try {
      eval("(string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testEvalMakeString() {
    assertEquals(new SCMMutableString(""), eval("(make-string 0)", env));
    assertEquals(new SCMMutableString(""), eval("(make-string 0 #\\a)", env));
    assertEquals(new SCMMutableString("a"), eval("(make-string 1 #\\a)", env));
    assertEquals(new SCMMutableString("aa"), eval("(make-string 2 #\\a)", env));
    assertEquals(new SCMMutableString("ZZZZZZZZ"), eval("(make-string 8 #\\Z)", env));
    assertEquals(new SCMMutableString("\u0000\u0000\u0000"), eval("(make-string 3)", env));
    assertEquals(new SCMMutableString("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"), eval("(make-string 8)", env));

    try {
      eval("(make-string \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: \"test\"",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }
    try {
      eval("(make-string 2 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: 1", e.getMessage());
    }
    try {
      eval("(make-string)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong number of arguments (actual: 0, expected: at least 1) passed to: make-string", e.getMessage());
    }
    try {
      eval("(make-string 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong number of arguments (3) passed to: make-string", e.getMessage());
    }
  }

  @Test
  public void testEvalStringFill() {
    try {
      eval("(string-fill! \"\" #\\a)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: MutableString, actual: \"\"", e.getMessage());
    }
    try {
      eval("(string-fill! \"z\" #\\a)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: MutableString, actual: \"z\"", e.getMessage());
    }
    try {
      eval("(string-fill! \"test1\" #\\a)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: MutableString, actual: \"test1\"", e.getMessage());
    }

    assertEquals(new SCMMutableString(""), eval("(string-fill! (make-string 0) #\\a)", env));
    assertEquals(new SCMMutableString("a"), eval("(string-fill! (make-string 1 #\\z) #\\a)", env));
    assertEquals(new SCMMutableString("aaaaa"), eval("(string-fill! (string #\\t #\\e #\\s #\\t #\\1) #\\a)", env));
  }

  @Test
  public void testEvalStringCopy() {
    assertEquals(new SCMMutableString(""), eval("(string-copy \"\")", env));
    assertEquals(new SCMMutableString("test"), eval("(string-copy \"test\")", env));
    assertEquals(new SCMMutableString("t"), eval("(string-copy \"t\")", env));
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
      assertEquals("Wrong argument type. Expected: String, actual: 1", e.getMessage());
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
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: -1",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }
    try {
      eval("(string-ref \"tes\" 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 3", e.getMessage());
    }
    try {
      eval("(string-ref \"\" 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 0", e.getMessage());
    }
    try {
      eval("(string-ref '(1 2 3) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: String, actual: (1 2 3)", e.getMessage());
    }
    try {
      eval("(string-ref \"test\" 0.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: 0.5",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }
  }

  @Test
  public void testEvalStringSet() {
    assertEquals(new SCMMutableString("z"),   eval("(let ((s (string #\\a) )) (string-set! s 0 #\\z) s)", env));
    assertEquals(new SCMMutableString("zbc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 0 #\\z) s)", env));
    assertEquals(new SCMMutableString("azc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 1 #\\z) s)", env));
    assertEquals(new SCMMutableString("abz"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 2 #\\z) s)", env));

    try {
      eval("(let ((s \"a\"  )) (string-set! s 0 #\\z) s)", env);
    } catch (WrongTypeException e) {
      assertEquals("Wrong argument type. Expected: MutableString, actual: \"a\"", e.getMessage());
    }

    try {
      eval("(string-set! (string #\\a #\\b #\\c) -1 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: -1",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }
    try {
      eval("(string-set! (string #\\a #\\b #\\c) 3 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 3", e.getMessage());
    }
    try {
      eval("(string-set! (make-string 0) 0 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Value out of range: 0", e.getMessage());
    }
    try {
      eval("(string-set! '(1 2 3) 2 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: MutableString, actual: (1 2 3)", e.getMessage());
    }
    try {
      eval("(string-set! (make-string 4) 0.5 #\\A)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals(String.format("Wrong argument type. Expected: %s, actual: 0.5",
                                 SCMClass.ExactNonNegativeInteger.class.getSimpleName()), e.getMessage());
    }
    try {
      eval("(string-set! (make-string 4) 3 '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: Character, actual: ()", e.getMessage());
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
      assertEquals("Wrong argument type. Expected: Symbol, actual: 1", e.getMessage());
    }
    try {
      eval("(string->symbol 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("Wrong argument type. Expected: String, actual: 1", e.getMessage());
    }
  }

  @Test
  public void testStringInterning() {
    // immutable interned strings must have the same hashcode
    assertEquals(TRUE, eval("(apply = (map hashcode '(\"test\" \"test\" \"test\" \"test\" \"test\")))", env));

    // immutable interned strings must point to the same object
    assertEquals(TRUE, eval("(eq? \"test\" \"test\" \"test\" \"test\" \"test\")", env));
    assertEquals(TRUE, eval("(eqv? \"test\" \"test\" \"test\" \"test\" \"test\")", env));
    assertEquals(TRUE, eval("(equal? \"test\" \"test\" \"test\" \"test\" \"test\")", env));
    // mutable interned strings
    assertEquals(FALSE, eval("(eq? (string #\\a) (string #\\a))", env));
    assertEquals(FALSE, eval("(eqv? (string #\\a) (string #\\a))", env));
    assertEquals(TRUE,  eval("(equal? (string #\\a) (string #\\a))", env));

    assertEquals(TRUE,  eval("(equal? \"a\" (string #\\a))", env));
    assertEquals(TRUE,  eval("(equal? (string #\\a) \"a\" )", env));

    assertEquals(TRUE,  eval("(mutable? (string #\\a))", env));
    assertEquals(FALSE, eval("(immutable? (string #\\a))", env));
    assertEquals(TRUE,  eval("(immutable? \"a\")", env));
    assertEquals(FALSE,  eval("(mutable? \"a\")", env));

    assertEquals(TRUE,  eval("(immutable? (string->immutable-string (string #\\a)))", env));
  }
}
