package unittests

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import java.math.BigDecimal
import java.util.Collections

class JavaInteropTest : AbstractTest() {

    @Test
    fun testJavaStaticFields() {
        assertEquals(Math.PI, eval("Math/PI"))
        assertTrue(Collections.EMPTY_LIST === eval("java.util.Collections/EMPTY_LIST"))
    }

    @Test
    fun testJavaStaticMethods() {
        System.setProperty("TESTKEY", "TESTVALUE")
        assertEquals("TESTVALUE", eval("(System/getProperty \"TESTKEY\")"))
        assertEquals(5.toShort(), eval("(Short/parseShort \"12\" 3)"))
    }

    @Test
    fun testJavaInstanceMethods() {
        assertEquals("FRED", eval("(.toUpperCase \"fred\")"))
        assertEquals(Long::class.javaObjectType, eval("(.getClass 5)"))
        assertEquals(Class::class.java, eval("(.getClass String)"))
        assertEquals(String::class.java, eval("(.getClass \"String\")"))
        eval("(define h (new java.util.HashMap))")
        eval("(.put h \"KEY\" \"VALUE\")")
        assertEquals("VALUE", eval("(.get h \"KEY\")"))
        assertEquals(5, eval("(.length (.get h \"KEY\"))"))
        eval("(.put h 1 1)")
        assertEquals(1L, eval("(.get h 1)"))
    }

    @Test
    fun testJavaClassMethods() {
        assertEquals("ring", eval("(.substring \"String\" 2)"))
        assertEquals("java.lang.String", eval("(.getName String)"))
        assertEquals("java.util.Collections", eval("(.getName java.util.Collections)"))
    }

    @Test
    fun testJavaDowncast() {
        assertEquals("es",   eval("(.substring \"test\" 1 3)"))
        assertEquals("zesz", eval("(.replace \"test\" #\\t #\\z)"))
        assertEquals(true,   eval("(.isEmpty \"\")"))
        assertEquals('e',    eval("(.charAt \"test\" 1)"))
        assertEquals(4,      eval("(.length \"test\")"))
    }

    @Test
    fun testJavaNewInstance() {
        assertEquals(1L,  eval("(new Long 1)"))
        assertEquals(6L,  eval("(new Long (+ 1 2 3))"))
        assertEquals(31L, eval("(new Long (.substring \"123123\" 2 4))"))
        assertEquals(BigDecimal.ONE, eval("(new java.math.BigDecimal 1)"))
        assertEquals(123, eval("(new Integer 123)"))
        assertEquals(1,   eval("(.signum (new java.math.BigDecimal 10))"))
    }

    @Test
    fun testIsClass() {
        assertEquals(true,  eval("(class? (class 123))"))
        assertEquals(true,  eval("(class? (class (new Object)))"))
        assertEquals(true,  eval("(class? (class +))"))
        assertEquals(false, eval("(class? 123)"))
        assertEquals(false, eval("(class? (new Object))"))
        assertEquals(false, eval("(class? +)"))
    }
}
