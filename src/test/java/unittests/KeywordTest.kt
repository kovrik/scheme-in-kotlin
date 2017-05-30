package unittests

import core.scm.Keyword
import org.junit.Assert.assertEquals
import org.junit.Test

class KeywordTest : AbstractTest() {

    @Test
    fun testEvalIsKeyword() {
        assertEquals(true,  eval("(keyword? :test)", env))
        assertEquals(true,  eval("(keyword? :a)", env))
        assertEquals(true,  eval("(keyword? :_)", env))
        assertEquals(true,  eval("(keyword? (keyword \"test\")", env))
        assertEquals(true,  eval("(keyword? (keyword \"a\"))", env))
        assertEquals(true,  eval("(keyword? (keyword \"_\"))", env))
        assertEquals(false, eval("(keyword? \"test\")", env))
        assertEquals(false, eval("(keyword? #\\a)", env))
        assertEquals(false, eval("(keyword? '())", env))
        assertEquals(false, eval("(keyword? [])", env))
        assertEquals(false, eval("(keyword? {})", env))
    }

    @Test
    fun testEvalKeywords() {
        assertEquals(Keyword.intern("a"), eval(":a", env))
        assertEquals(Keyword.intern("test"), eval(":test", env))
        assertEquals(Keyword.intern("_"), eval(":_", env))
    }

    @Test
    fun testEvalKeywordsAsFunctions() {
        assertEquals(1L,   eval("(:a {:a 1})", env))
        assertEquals(null, eval("(:c {:a 1})", env))
        assertEquals(2L,   eval("(:c {:a 1} 2)", env))
        assertEquals(9L,   eval("(:c {:a 1, :c 9} 123)", env))
    }
}
