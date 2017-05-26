package unittests

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import java.lang.Boolean.TRUE
import java.math.BigDecimal
import java.util.*

class JavaInteropTest : AbstractTest() {

    @Test
    fun testJavaStaticFields() {
        assertEquals(Math.PI, eval("Math/PI", env))
        assertTrue(Collections.EMPTY_LIST === eval("java.util.Collections/EMPTY_LIST", env))
    }

    @Test
    fun testJavaStaticMethods() {
        System.setProperty("TESTKEY", "TESTVALUE")
        assertEquals("TESTVALUE", eval("(System/getProperty \"TESTKEY\")", env))
        assertEquals(java.lang.Short.valueOf("5"), eval("(Short/parseShort \"12\" 3)", env))
    }

    @Test
    fun testJavaInstanceMethods() {
        assertEquals("FRED", eval("(.toUpperCase \"fred\")", env))
        assertEquals(Long::class.javaObjectType, eval("(.getClass 5)", env))
        assertEquals(Class::class.java, eval("(.getClass String)", env))
        assertEquals(String::class.java, eval("(.getClass \"String\")", env))
        val map = "(define h (new java.util.HashMap))"
        eval(map, env)
        eval("(.put h \"KEY\" \"VALUE\")", env)
        assertEquals("VALUE", eval("(.get h \"KEY\")", env))
        assertEquals(5, eval("(.length (.get h \"KEY\"))", env))
        eval("(.put h 1 1)", env)
        assertEquals(1L, eval("(.get h 1)", env))
    }

    @Test
    fun testJavaClassMethods() {
        assertEquals("ring", eval("(.substring \"String\" 2)", env))
        assertEquals("java.lang.String", eval("(.getName String)", env))
        assertEquals("java.util.Collections", eval("(.getName java.util.Collections)", env))
    }

    @Test
    fun testJavaDowncast() {
        assertEquals("es", eval("(.substring \"test\" 1 3)", env))
        assertEquals("zesz", eval("(.replace \"test\" #\\t #\\z)", env))
        assertEquals(TRUE, eval("(.isEmpty \"\")", env))
        assertEquals('e', eval("(.charAt \"test\" 1)", env))
        assertEquals(4, eval("(.length \"test\")", env))
    }

    @Test
    fun testJavaNewInstance() {
        assertEquals(1L, eval("(new Long 1)", env))
        assertEquals(6L, eval("(new Long (+ 1 2 3))", env))
        assertEquals(31L, eval("(new Long (.substring \"123123\" 2 4))", env))
        assertEquals(BigDecimal.ONE, eval("(new java.math.BigDecimal 1)", env))
        assertEquals(123, eval("(new Integer 123)", env))
        assertEquals(1, eval("(.signum (new java.math.BigDecimal 10))", env))
    }
}
