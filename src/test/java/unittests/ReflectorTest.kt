package unittests

import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException
import org.junit.Assert.*
import org.junit.Test
import java.math.BigDecimal
import java.math.BigInteger
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class ReflectorTest : AbstractTest() {

    @Test
    fun testEvalDot() {
        try {
            eval(".")
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
    }

    @Test
    fun testEvalStaticMethods() {
        assertTrue((eval("(java.util.Collections/emptyList)") as List<*>).isEmpty())
        assertTrue((eval("(. java.util.Collections emptyList)") as List<*>).isEmpty())
        assertEquals(-15L, eval("(Long/valueOf -15)"))
        assertEquals(-15L, eval("(. Long valueOf -15)"))
        try {
            eval("(Longzz/valueOf -15)")
            fail()
        } catch (e: ClassNotFoundException) {
            // expected
        }
        try {
            eval("(. Longzzz valueOf -15)")
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }
        try {
            eval("(Long/ 1)")
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
        try {
            eval("(. Class getName)")
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
    }

    @Test
    fun testEvalStaticFields() {
        assertEquals(BigDecimal.ONE, eval("java.math.BigDecimal/ONE"))
        assertEquals(BigDecimal.ONE, eval("BigDecimal/ONE"))
        assertEquals(BigInteger.ONE, eval("BigInteger/ONE"))
        assertEquals(Math.PI, eval("Math/PI"))
        assertEquals(Math.PI, eval("(. java.lang.Math PI)"))
        try {
            eval("Math/")
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
        try {
            eval("Math/BOOM")
            fail()
        } catch (e: NoSuchFieldException) {
            // expected
        }
        try {
            eval("java.awt.Point/x")
            fail()
        } catch (e: NoSuchFieldException) {
            // expected
        }
    }

    @Test
    fun testEvalMemberFields() {
        eval("(def point (new java.awt.Point 15 4))")
        assertEquals(15, eval("(.-x point)"))
        assertEquals(4, eval("(.-y point)"))
        try {
            eval("(.-z point)")
            fail()
        } catch (e: NoSuchFieldException) {
            // expected
        }
    }

    @Test
    fun testEvalMemberMethods() {
        assertEquals(String::class.java, eval("(.getClass \"\")"))
        assertEquals(String::class.java, eval("(. \"\" getClass)"))
        assertEquals(Long::class.javaObjectType, eval("(.getClass 1)"))
        assertEquals(Long::class.javaObjectType, eval("(. 1 getClass)"))
        assertEquals(Long::class.javaObjectType, eval("(. (+ 2 3 4) getClass)"))
        assertEquals("123", eval("(.toString 123)"))
        assertEquals(1, eval("(. (+ 1 2 3) compareTo (+ 1 2))"))
        assertNull(eval("(.get (new java.util.HashMap) (.get (new java.util.HashMap) 1))"))
        try {
            eval("(.getClass nil)")
            fail()
        } catch (e: NullPointerException) {
            // expected
        }
        try {
            eval("(.toString nil)")
            fail()
        } catch (e: NullPointerException) {
            // expected
        }
        try {
            eval("(.toStringz 123)")
            fail()
        } catch (e: NoSuchMethodException) {
            // expected
        }
        try {
            eval("(.-toString)")
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
        try {
            eval("(.toString)")
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
    }

    @Test
    fun testEvalConstructors() {
        assertNotNull(eval("(new Object)"))
        assertNotNull(eval("(Object.)"))
        assertEquals("123", eval("(new String \"123\")"))
        assertEquals("123", eval("(String. \"123\")"))
        try {
            eval("(String. (new Object))")
            fail()
        } catch (e: NoSuchMethodException) {
            // expected
        }
    }

    @Test
    fun testJavaTypes() {
        assertEquals((-123).toShort(), eval("(short -123)"))
        assertEquals((-123).toByte(), eval("(byte -123)"))
        assertEquals((-123).toInt(), eval("(int -123)"))
        assertEquals((-123).toLong(), eval("(long -123)"))
        assertEquals((-123).toFloat(), eval("(float -123)"))
        assertEquals((-123).toDouble(), eval("(double -123)"))
        assertEquals(98.toChar(), eval("(char 98)"))
        assertEquals(BigInteger("-123"), eval("(bigint -123)"))
        assertEquals(BigDecimal("-123.456"), eval("(bigdec -123.456)"))
        assertTrue(eval("(boolean 98)") as Boolean)
    }

    @Test
    fun testIsAccessible() {
        assertEquals(1L, eval("(let ((a (new java.util.ArrayList)))" +
                              "  (.add a 1)" +
                              "  (.add a 2)" +
                              "  (.add a 2)" +
                              "  (.next (.iterator a)))"))
    }
}
