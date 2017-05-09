package unittests;

import core.exceptions.WrongTypeException;
import core.scm.MutableString;
import core.scm.Symbol;
import core.scm.Vector;
import core.writer.Writer;
import org.junit.Test;

import static core.scm.Cons.list;
import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
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
    assertEquals("Lorem \"ipsum\" ", eval("\"Lorem \\\"ipsum\\\" \"", env));
    assertEquals("", eval("\"\"", env));
    assertEquals(1, eval("(count \"\\\"\")", env));
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
    assertEquals(new MutableString(""), eval("(string)", env));
    assertEquals(new MutableString("a"), eval("(string #\\a)", env));
    assertEquals(new MutableString("abc"), eval("(string #\\a #\\b #\\c)", env));

    try {
      eval("(string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testEvalMakeString() {
    assertEquals(new MutableString(""), eval("(make-string 0)", env));
    assertEquals(new MutableString(""), eval("(make-string 0 #\\a)", env));
    assertEquals(new MutableString("a"), eval("(make-string 1 #\\a)", env));
    assertEquals(new MutableString("aa"), eval("(make-string 2 #\\a)", env));
    assertEquals(new MutableString("ZZZZZZZZ"), eval("(make-string 8 #\\Z)", env));
    assertEquals(new MutableString("\u0000\u0000\u0000"), eval("(make-string 3)", env));
    assertEquals(new MutableString("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"), eval("(make-string 8)", env));

    try {
      eval("(make-string \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("make-string: type mismatch; (expected: ExactNonNegativeInteger, given: \"test\")", e.getMessage());
    }
    try {
      eval("(make-string 2 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("make-string: type mismatch; (expected: Character, given: 1)", e.getMessage());
    }
    try {
      eval("(make-string)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("make-string: arity mismatch; the expected number of arguments does not match the given number (expected: 1 to 2, given: 0)", e.getMessage());
    }
    try {
      eval("(make-string 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("make-string: arity mismatch; the expected number of arguments does not match the given number (expected: 1 to 2, given: 3)", e.getMessage());
    }
  }

  @Test
  public void testEvalStringFill() {
    try {
      eval("(string-fill! \"\" #\\a)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("string-fill!: type mismatch; (expected: MutableString, given: \"\")", e.getMessage());
    }
    try {
      eval("(string-fill! \"z\" #\\a)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("string-fill!: type mismatch; (expected: MutableString, given: \"z\")", e.getMessage());
    }
    try {
      eval("(string-fill! \"test1\" #\\a)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("string-fill!: type mismatch; (expected: MutableString, given: \"test1\")", e.getMessage());
    }

    assertEquals(new MutableString(""), eval("(string-fill! (make-string 0) #\\a)", env));
    assertEquals(new MutableString("a"), eval("(string-fill! (make-string 1 #\\z) #\\a)", env));
    assertEquals(new MutableString("aaaaa"), eval("(string-fill! (string #\\t #\\e #\\s #\\t #\\1) #\\a)", env));
  }

  @Test
  public void testEvalStringCopy() {
    assertEquals(new MutableString(""), eval("(string-copy \"\")", env));
    assertEquals(new MutableString("test"), eval("(string-copy \"test\")", env));
    assertEquals(new MutableString("t"), eval("(string-copy \"t\")", env));
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
      assertEquals("string-length: type mismatch; (expected: String, given: 1)", e.getMessage());
    }
  }

  @Test
  public void testStringToList() {
    assertEquals(list('a', 'b', 'c'), eval("(string->list \"abc\")", env));
    assertEquals(list('a'), eval("(string->list \"a\")", env));
    assertEquals(list(), eval("(string->list \"\")", env));
    try {
      eval("(string->list (cons 1 2))", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string->list: type mismatch; (expected: String, given: (1 . 2))", e.getMessage());
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
      assertEquals("string-ref: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.getMessage());
    }
    try {
      eval("(string-ref \"tes\" 3)", env);
      fail();
    } catch (IndexOutOfBoundsException e) {
      assertEquals("string-ref: value out of range: 3", e.getMessage());
    }
    try {
      eval("(string-ref \"\" 0)", env);
      fail();
    } catch (IndexOutOfBoundsException e) {
      assertEquals("string-ref: value out of range: 0", e.getMessage());
    }
    try {
      eval("(string-ref '(1 2 3) 0)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string-ref: type mismatch; (expected: String, given: (1 2 3))", e.getMessage());
    }
    try {
      eval("(string-ref \"test\" 0.5)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string-ref: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.getMessage());
    }
  }

  @Test
  public void testEvalStringSet() {
    assertEquals(new MutableString("z"), eval("(let ((s (string #\\a) )) (string-set! s 0 #\\z) s)", env));
    assertEquals(new MutableString("zbc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 0 #\\z) s)", env));
    assertEquals(new MutableString("azc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 1 #\\z) s)", env));
    assertEquals(new MutableString("abz"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 2 #\\z) s)", env));

    try {
      eval("(let ((s \"a\"  )) (string-set! s 0 #\\z) s)", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("string-set!: type mismatch; (expected: MutableString, given: \"a\")", e.getMessage());
    }

    try {
      eval("(string-set! (string #\\a #\\b #\\c) -1 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string-set!: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.getMessage());
    }
    try {
      eval("(string-set! (string #\\a #\\b #\\c) 3 #\\z)", env);
      fail();
    } catch (IndexOutOfBoundsException e) {
      assertEquals("string-set!: value out of range: 3", e.getMessage());
    }
    try {
      eval("(string-set! (make-string 0) 0 #\\z)", env);
      fail();
    } catch (IndexOutOfBoundsException e) {
      assertEquals("string-set!: value out of range: 0", e.getMessage());
    }
    try {
      eval("(string-set! '(1 2 3) 2 #\\z)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string-set!: type mismatch; (expected: MutableString, given: (1 2 3))", e.getMessage());
    }
    try {
      eval("(string-set! (make-string 4) 0.5 #\\A)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string-set!: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.getMessage());
    }
    try {
      eval("(string-set! (make-string 4) 3 '())", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string-set!: type mismatch; (expected: Character, given: ())", e.getMessage());
    }
  }

  @Test
  public void testEvalSymbolStringConversion() {
    assertEquals("test",  eval("(symbol->string 'test)", env));
    assertEquals("test",  eval("(symbol->string (string->symbol (symbol->string 'test)))", env));
    assertEquals("TeSt",  eval("(symbol->string (string->symbol (symbol->string 'TeSt)))", env));
    assertEquals("",    eval("(symbol->string (string->symbol \"\"))", env));
    assertEquals("123", eval("(symbol->string (string->symbol \"123\"))", env));
    assertEquals("6",   eval("(symbol->string (string->symbol \"6\"))", env));
    assertEquals("6bsdf", eval("(symbol->string (string->symbol \"6bsdf\"))", env));
    assertEquals("one two three", eval("(symbol->string (string->symbol \"one two three\"))", env));
    assertEquals("  ", eval("(symbol->string (string->symbol \"  \"))", env));
    assertEquals("||",    Writer.write(eval("(string->symbol \"\")", env)));
    assertEquals("|123|", Writer.write(eval("(string->symbol \"123\")", env)));
    assertEquals("|6|",   Writer.write(eval("(string->symbol \"6\")", env)));
    assertEquals("|6bsdf|", Writer.write(eval("(string->symbol \"6bsdf\")", env)));
    assertEquals("|one two three|", Writer.write(eval("(string->symbol \"one two three\")", env)));
    assertEquals("|  |", Writer.write(eval("(string->symbol \"  \")", env)));
//    assertEquals("|.|", Writer.write(eval("(string->symbol \".\")", env)));
    assertEquals("test.", Writer.write(eval("(string->symbol \"test.\")", env)));
    assertEquals("|#|", Writer.write(eval("(string->symbol \"#\")", env)));
    assertEquals("|#123|", Writer.write(eval("(string->symbol \"#123\")", env)));
    assertEquals("|#abc|", Writer.write(eval("(string->symbol \"#abc\")", env)));
    assertEquals("a#bc", Writer.write(eval("(string->symbol \"a#bc\")", env)));
    assertEquals("abc#", Writer.write(eval("(string->symbol \"abc#\")", env)));
    assertEquals("#%abc", Writer.write(eval("(string->symbol \"#%abc\")", env)));
    assertEquals("(|a b c|)", Writer.write(eval("(list (string->symbol \"a b c\"))", env)));
    assertEquals(Symbol.intern("test"), eval("(string->symbol (symbol->string 'test))", env));
    try {
      eval("(symbol->string 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("symbol->string: type mismatch; (expected: Symbol, given: 1)", e.getMessage());
    }
    try {
      eval("(string->symbol 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("string->symbol: type mismatch; (expected: String, given: 1)", e.getMessage());
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
    assertEquals(FALSE, eval("(mutable? \"a\")", env));
    assertEquals(TRUE,  eval("(immutable? (string->immutable-string (string #\\a)))", env));
  }

  @Test
  public void testIsBlank() {
    assertEquals(TRUE,  eval("(blank? nil)", env));
    assertEquals(TRUE,  eval("(blank? \"\")", env));
    assertEquals(TRUE,  eval("(blank? \"      \")", env));
    assertEquals(TRUE,  eval("(blank? \" \t\t\r\n    \")", env));
    assertEquals(FALSE, eval("(blank? \" \t\ta\r\n    \")", env));
    assertEquals(FALSE, eval("(blank? \"not-blank\")", env));
  }

  @Test
  public void testSplit() {
    assertEquals(3,  ((Vector)eval("(split \"a,b,c,\" #\",\")", env)).length());
    assertEquals(1,  ((Vector)eval("(split \"a,b,c,\" #\"_\")", env)).length());
    assertEquals(4,  ((Vector)eval("(split \"a,b,c,\" #\",\" -1)", env)).length());
    assertEquals(10, ((Vector)eval("(split \"q1w2e3r4t5y6u7i8o9p0\" #\"\\d+\")", env)).length());
    assertEquals(5, ((Vector)eval("(split \"q1w2e3r4t5y6u7i8o9p0\" #\"\\d+\" 5)", env)).length());
  }
}
