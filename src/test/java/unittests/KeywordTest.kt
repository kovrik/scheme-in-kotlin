package unittests

import core.scm.Keyword
import org.junit.Assert.assertEquals
import org.junit.Test

class KeywordTest : AbstractTest() {

    @Test
    fun testEvalIsKeyword() {
        assertEquals(true,  eval("(keyword? :test)"))
        assertEquals(true,  eval("(keyword? :a)"))
        assertEquals(true,  eval("(keyword? :_)"))
        assertEquals(true,  eval("(keyword? (keyword \"test\")"))
        assertEquals(true,  eval("(keyword? (keyword \"a\"))"))
        assertEquals(true,  eval("(keyword? (keyword \"_\"))"))
        assertEquals(false, eval("(keyword? \"test\")"))
        assertEquals(false, eval("(keyword? #\\a)"))
        assertEquals(false, eval("(keyword? '())"))
        assertEquals(false, eval("(keyword? [])"))
        assertEquals(false, eval("(keyword? {})"))
    }

    @Test
    fun testEvalKeywords() {
        assertEquals(Keyword.intern("a"), eval(":a"))
        assertEquals(Keyword.intern("test"), eval(":test"))
        assertEquals(Keyword.intern("_"), eval(":_"))
    }

    @Test
    fun testEvalKeywordsAsFunctions() {
        assertEquals(1L,   eval("(:a {:a 1})"))
        assertEquals(null, eval("(:c {:a 1})"))
        assertEquals(2L,   eval("(:c {:a 1} 2)"))
        assertEquals(9L,   eval("(:c {:a 1, :c 9} 123)"))
    }
}
