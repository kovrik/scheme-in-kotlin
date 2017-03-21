package unittests.s7.tests;

import core.exceptions.IllegalSyntaxException;
import org.junit.Test;
import unittests.AbstractTest;

import static junit.framework.TestCase.assertEquals;
import static java.lang.Boolean.TRUE;
import static java.lang.Boolean.FALSE;
import static org.junit.Assert.fail;

public class IsSymbolTest extends AbstractTest {

  @Test
  public void testIsSymbol() {
    String[] trues = {"(symbol? 't)  ", "(symbol? 'foo)", "(symbol? (car '(a b)))", "(symbol? 'nil)", "(symbol? 'car)",
        "(symbol? ':)", "(symbol? '|)", "(symbol? '|')", "(symbol? '@)",
        "(symbol? 'sym0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789)",
        "(symbol? (vector-ref #(1 a 34) 1))", "(symbol? (string->symbol \"if\"))", "(symbol? 'quote)",
        "(symbol? 'begin)", "(symbol? 'if)", "(if (symbol? '1+) (symbol? '0e) #t)", "(if (symbol? '1+) (symbol? '0000eeesve) #t)",};
    assertAllEqual(TRUE, trues, env);

    String[] falses = {"(symbol? '(AB\\c () xyz))", "(symbol? #b1)", "(symbol? car)", "(symbol? '#f)", "(symbol? #())",
        "(symbol? '())", "(symbol? #())", "(symbol? #f)", "(symbol? \"t\") ", "(symbol? '(t))", "(symbol? #t)  ", "(symbol? 4)   ",};
    assertAllEqual(FALSE, falses, env);

    try {
      eval("(if (symbol? '1+) (symbol? '#xff0000eeesve) #t)", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad exponent: ff0000eeesve", e.getMessage());
    }
  }
}
