package unittests

import core.scm.Keyword
import org.junit.Test

import java.lang.Boolean.FALSE
import java.lang.Boolean.TRUE
import org.junit.Assert.assertEquals

class KeywordTest : AbstractTest() {

    @Test
    fun testEvalIsKeyword() {
        assertEquals(TRUE, eval("(keyword? :test)", env))
        assertEquals(TRUE, eval("(keyword? :a)", env))
        assertEquals(TRUE, eval("(keyword? :_)", env))
        assertEquals(TRUE, eval("(keyword? (keyword \"test\")", env))
        assertEquals(TRUE, eval("(keyword? (keyword \"a\"))", env))
        assertEquals(TRUE, eval("(keyword? (keyword \"_\"))", env))
        assertEquals(FALSE, eval("(keyword? \"test\")", env))
        assertEquals(FALSE, eval("(keyword? #\\a)", env))
        assertEquals(FALSE, eval("(keyword? '())", env))
        assertEquals(FALSE, eval("(keyword? [])", env))
        assertEquals(FALSE, eval("(keyword? {})", env))
    }

    @Test
    fun testEvalKeywords() {
        assertEquals(Keyword.intern("a"), eval(":a", env))
        assertEquals(Keyword.intern("test"), eval(":test", env))
        assertEquals(Keyword.intern("_"), eval(":_", env))
    }

    @Test
    fun testEvalKeywordsAsFunctions() {
        assertEquals(1L, eval("(:a {:a 1})", env))
        assertEquals(null, eval("(:c {:a 1})", env))
        assertEquals(2L, eval("(:c {:a 1} 2)", env))
        assertEquals(9L, eval("(:c {:a 1, :c 9} 123)", env))
    }
}
