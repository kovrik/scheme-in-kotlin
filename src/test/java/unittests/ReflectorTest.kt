package unittests

import core.exceptions.IllegalSyntaxException
import core.exceptions.UndefinedIdentifierException
import org.junit.Test

import java.math.BigDecimal
import java.math.BigInteger

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail

class ReflectorTest : AbstractTest() {

    @Test
    fun testEvalDot() {
        try {
            eval(".", env)
        } catch (e: IllegalSyntaxException) {
            // expected
        }
    }

    @Test
    fun testEvalStaticMethods() {
        assertTrue((eval("(java.util.Collections/emptyList)", env) as List<*>).isEmpty())
        assertTrue((eval("(. java.util.Collections emptyList)", env) as List<*>).isEmpty())
        assertEquals(-15L, eval("(Long/valueOf -15)", env))
        assertEquals(-15L, eval("(. Long valueOf -15)", env))
        try {
            eval("(Longzz/valueOf -15)", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }

        try {
            eval("(. Longzzz valueOf -15)", env)
            fail()
        } catch (e: UndefinedIdentifierException) {
            // expected
        }

        try {
            eval("(Long/ 1)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }

        try {
            eval("(. Class getName)", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
    }

    @Test
    fun testEvalStaticFields() {
        assertEquals(BigDecimal.ONE, eval("java.math.BigDecimal/ONE", env))
        assertEquals(BigDecimal.ONE, eval("BigDecimal/ONE", env))
        assertEquals(BigInteger.ONE, eval("BigInteger/ONE", env))
        assertEquals(Math.PI, eval("Math/PI", env))
        assertEquals(Math.PI, eval("(. java.lang.Math PI)", env))
        try {
            eval("Math/", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }

        try {
            eval("Math/BOOM", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }

        try {
            eval("java.awt.Point/x", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
    }

    @Test
    fun testEvalMemberFields() {
        eval("(def point (new java.awt.Point 15 4))", env)
        assertEquals(15, eval("(.-x point)", env))
        assertEquals(4, eval("(.-y point)", env))
        try {
            eval("(.-z point)", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
    }

    @Test
    fun testEvalMemberMethods() {
        assertEquals(String::class.java, eval("(.getClass \"\")", env))
        assertEquals(String::class.java, eval("(. \"\" getClass)", env))
        assertEquals(Long::class.javaObjectType, eval("(.getClass 1)", env))
        assertEquals(Long::class.javaObjectType, eval("(. 1 getClass)", env))
        assertEquals(Long::class.javaObjectType, eval("(. (+ 2 3 4) getClass)", env))
        assertEquals("123", eval("(.toString 123)", env))
        assertEquals(1, eval("(. (+ 1 2 3) compareTo (+ 1 2))", env))
        try {
            eval("(.getClass nil)", env)
            fail()
        } catch (e: NullPointerException) {
            // expected
        }

        try {
            eval("(.toString nil)", env)
            fail()
        } catch (e: NullPointerException) {
            // expected
        }

        try {
            eval("(.toStringz 123)", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }

        try {
            eval("(.-toString)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }

        try {
            eval("(.toString)", env)
            fail()
        } catch (e: IllegalSyntaxException) {
            // expected
        }
    }

    @Test
    fun testEvalConstructors() {
        assertTrue(eval("(new Object)", env) != null)
        assertTrue(eval("(Object.)", env) != null)
        assertEquals("123", eval("(new String \"123\")", env))
        assertEquals("123", eval("(String. \"123\")", env))
        try {
            eval("(String. (new Object))", env)
            fail()
        } catch (e: RuntimeException) {
            // expected
        }
    }

    @Test
    fun testJavaTypes() {
        assertEquals((-123).toShort(), eval("(short -123)", env))
        assertEquals((-123).toByte(), eval("(byte -123)", env))
        assertEquals((-123).toInt(), eval("(int -123)", env))
        assertEquals((-123).toLong(), eval("(long -123)", env))
        assertEquals((-123).toFloat(), eval("(float -123)", env))
        assertEquals((-123).toDouble(), eval("(double -123)", env))
        assertEquals(98.toChar(), eval("(char 98)", env))
        assertEquals(BigInteger("-123"), eval("(bigint -123)", env))
        assertEquals(BigDecimal("-123.456"), eval("(bigdec -123.456)", env))
        assertTrue(eval("(boolean 98)", env) as Boolean)
    }
}
