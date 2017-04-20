package unittests;

import core.scm.SCMKeyword;
import org.junit.Test;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.assertEquals;

public class KeywordTest extends AbstractTest {

  @Test
  public void testEvalIsKeyword() {
    assertEquals(TRUE,  eval("(keyword? :test)", env));
    assertEquals(TRUE,  eval("(keyword? :a)", env));
    assertEquals(TRUE,  eval("(keyword? :_)", env));
    assertEquals(TRUE,  eval("(keyword? (keyword \"test\")", env));
    assertEquals(TRUE,  eval("(keyword? (keyword \"a\"))", env));
    assertEquals(TRUE,  eval("(keyword? (keyword \"_\"))", env));
    assertEquals(FALSE, eval("(keyword? \"test\")", env));
    assertEquals(FALSE, eval("(keyword? #\\a)", env));
    assertEquals(FALSE, eval("(keyword? '())", env));
    assertEquals(FALSE, eval("(keyword? [])", env));
    assertEquals(FALSE, eval("(keyword? {})", env));
  }

  @Test
  public void testEvalKeywords() {
    assertEquals(SCMKeyword.intern("a"), eval(":a", env));
    assertEquals(SCMKeyword.intern("test"), eval(":test", env));
    assertEquals(SCMKeyword.intern("_"), eval(":_", env));
  }

  @Test
  public void testEvalKeywordsAsFunctions() {
    assertEquals(1L,   eval("(:a {:a 1})", env));
    assertEquals(null, eval("(:c {:a 1})", env));
    assertEquals(2L,   eval("(:c {:a 1} 2)", env));
    assertEquals(9L,   eval("(:c {:a 1, :c 9} 123)", env));
  }
}
