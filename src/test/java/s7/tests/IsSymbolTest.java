package s7.tests;

import org.junit.Test;

import static core.scm.SCMBoolean.FALSE;
import static core.scm.SCMBoolean.TRUE;
import static junit.framework.TestCase.assertEquals;

public class IsSymbolTest extends AbstractS7Test {

  @Test
  public void testIsSymbol() {
    assertEquals(TRUE, eval("(symbol? 't)  ", env));
    assertEquals(FALSE, eval("(symbol? \"t\") ", env));
    assertEquals(FALSE, eval("(symbol? '(t))", env));
    assertEquals(FALSE, eval("(symbol? #t)  ", env));
    assertEquals(FALSE, eval("(symbol? 4)   ", env));
    assertEquals(TRUE, eval("(symbol? 'foo)", env));
    assertEquals(TRUE, eval("(symbol? (car '(a b)))", env));
    assertEquals(TRUE, eval("(symbol? 'nil)", env));
    assertEquals(FALSE, eval("(symbol? '())", env));
    assertEquals(FALSE, eval("(symbol? #())", env));
    assertEquals(FALSE, eval("(symbol? #f)", env));
    assertEquals(TRUE, eval("(symbol? 'car)", env));
    assertEquals(FALSE, eval("(symbol? car)", env));
    assertEquals(FALSE, eval("(symbol? '#f)", env));
    assertEquals(FALSE, eval("(symbol? #())", env));
    assertEquals(TRUE, eval("(symbol? ':)", env));
    assertEquals(TRUE, eval("(symbol? '|)", env));
    assertEquals(TRUE, eval("(symbol? '|')", env));
    assertEquals(TRUE, eval("(symbol? '@)", env));
    assertEquals(TRUE, eval("(symbol? 'sym0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789)", env));
    assertEquals(TRUE, eval("(symbol? (vector-ref #(1 a 34) 1))", env));
    assertEquals(TRUE, eval("(if (symbol? '1+) (symbol? '0e) #t)", env));
    assertEquals(TRUE, eval("(symbol? (string->symbol \"if\"))", env));
    assertEquals(TRUE, eval("(symbol? 'quote)", env));
    assertEquals(FALSE, eval("(symbol? '(AB\\c () xyz))", env));
    assertEquals(TRUE, eval("(symbol? 'begin)", env));
    assertEquals(TRUE, eval("(symbol? 'if)", env));
    assertEquals(FALSE, eval("(symbol? #b1)", env));
  }
}
