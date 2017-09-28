package unittests

import core.scm.BigComplex
import org.junit.Test

import java.math.BigDecimal

import org.junit.Assert.assertEquals

class TrigonometryTest : AbstractTest() {

    // FIXME precision for big numbers

    @Test
    fun testSin() {
        assertEquals(0L, eval("(sin 0)", env))
        assertEquals(0.8414709848078965, eval("(sin 1)", env))
        assertEquals(-0.8414709848078965, eval("(sin -1)", env))
        assertEquals(0.8414709848078965, eval("(sin 1/1)", env))
        assertEquals(0.8414709848078965, eval("(sin 1.0)", env))
        assertEquals(1.2246467991473532E-16, eval("(sin pi)", env))
        assertEquals(1.0, eval("(sin (/ pi 2))", env))
        assertEquals(0.7457052121767203, eval("(sin 2.3)", env))
        assertEquals(BigComplex(BigDecimal("1.2984575814159773"), BigDecimal("0.6349639147847361")), eval("(sin 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("-8.471645454300148"), BigDecimal("-5.412680923178193")), eval("(sin -1-3i)", env))
//            assertEquals(-0.1285197485957309, eval("(sin 99999999999999999999999999999999999999999999999999999999999999)", env))
    }

    @Test
    fun testCos() {
        assertEquals(1L, eval("(cos 0)", env))
        assertEquals(0.5403023058681398, eval("(cos 1)", env))
        assertEquals(0.5403023058681398, eval("(cos -1)", env))
        assertEquals(0.5403023058681398, eval("(cos 1/1)", env))
        assertEquals(0.5403023058681398, eval("(cos 1.0)", env))
        assertEquals(-1.0, eval("(cos pi)", env))
        assertEquals(-0.666276021279824, eval("(cos 2.3)", env) as Double, 0.000000000000001)
        assertEquals(BigComplex(BigDecimal("0.8337300251311492"), BigDecimal("-0.9888977057628651")), eval("(cos 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("5.439580991019764"), BigDecimal("-8.429751080849945")), eval("(cos -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(cos 99999999999999999999999999999999999999999999999999999999999999)", env));
    }

    @Test
    fun testTan() {
        assertEquals(0L, eval("(tan 0)", env))
        assertEquals(1.5574077246549023, eval("(tan 1)", env))
        assertEquals(-1.5574077246549023, eval("(tan -1)", env))
        assertEquals(1.5574077246549023, eval("(tan 1/1)", env))
        assertEquals(1.5574077246549023, eval("(tan 1.0)", env))
        assertEquals(-1.2246467991473532e-016, eval("(tan pi)", env))
        assertEquals(-1.1192136417341325, eval("(tan 2.3)", env))
        assertEquals(BigComplex(BigDecimal("0.2717525853195118"), BigDecimal("1.083923327338694")), eval("(tan 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("-0.0045171372766583"), BigDecimal("-1.002054988245812")), eval("(tan -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(tan 99999999999999999999999999999999999999999999999999999999999999)", env));
    }

    @Test
    fun testSinh() {
        assertEquals(0L, eval("(sinh 0)", env))
        assertEquals(1.1752011936438014, eval("(sinh 1)", env))
        assertEquals(-1.1752011936438014, eval("(sinh -1)", env))
        assertEquals(1.1752011936438014, eval("(sinh 1/1)", env))
        assertEquals(1.1752011936438014, eval("(sinh 1.0)", env))
        assertEquals(11.548739357257748, eval("(sinh pi)", env))
        assertEquals(2.3012989023072947, eval("(sinh (/ pi 2))", env))
        assertEquals(4.936961805545957, eval("(sinh 2.3)", env))
        assertEquals(BigComplex(BigDecimal("0.6349639147847361"), BigDecimal("1.2984575814159773")), eval("(sinh 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("1.1634403637032504"), BigDecimal("-0.21775955162215221")), eval("(sinh -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(sinh 99999999999999999999999999999999999999999999999999999999999999)", env));
    }

    @Test
    fun testCosh() {
        assertEquals(1L, eval("(cosh 0)", env))
        assertEquals(1.543080634815244, eval("(cosh 1)", env))
        assertEquals(1.543080634815244, eval("(cosh -1)", env))
        assertEquals(1.543080634815244, eval("(cosh 1/1)", env))
        assertEquals(1.543080634815244, eval("(cosh 1.0)", env))
        assertEquals(11.591953275521519, eval("(cosh pi)", env))
        assertEquals(5.037220649268761, eval("(cosh 2.3)", env))
        assertEquals(BigComplex(BigDecimal("0.8337300251311492"), BigDecimal("0.9888977057628651")), eval("(cosh 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("-1.5276382501165435"), BigDecimal("0.1658444019189788")), eval("(cosh -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(cosh 99999999999999999999999999999999999999999999999999999999999999)", env));
    }

    @Test
    fun testTanh() {
        assertEquals(0L, eval("(tanh 0)", env))
        assertEquals(0.7615941559557649, eval("(tanh 1)", env))
        assertEquals(-0.7615941559557649, eval("(tanh -1)", env))
        assertEquals(0.7615941559557649, eval("(tanh 1/1)", env))
        assertEquals(0.7615941559557649, eval("(tanh 1.0)", env))
        assertEquals(0.99627207622075, eval("(tanh pi)", env))
        assertEquals(0.9800963962661914, eval("(tanh 2.3)", env))
        assertEquals(BigComplex(BigDecimal("1.083923327338694"), BigDecimal("0.2717525853195118")), eval("(tanh 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("-0.7680176472869111"), BigDecimal("0.05916853956605073")), eval("(tanh -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(tanh 99999999999999999999999999999999999999999999999999999999999999)", env));
    }

    @Test
    fun testAsin() {
        assertEquals(0L, eval("(asin 0)", env))
        assertEquals(1.5707963267948966, eval("(asin 1)", env))
        assertEquals(-1.5707963267948966, eval("(asin -1)", env))
        assertEquals(1.5707963267948966, eval("(asin 1/1)", env))
        assertEquals(1.5707963267948966, eval("(asin 1.0)", env))
        assertEquals(BigComplex(BigDecimal("1.5707963267948966"), BigDecimal("-1.8115262724608532")), eval("(asin pi)", env))
        assertEquals(BigComplex(BigDecimal("1.5707963267948966"), BigDecimal("-1.0232274785475506")), eval("(asin (/ pi 2))", env))
        assertEquals(BigComplex(BigDecimal("1.5707963267948966"), BigDecimal("-1.4750447812414251")), eval("(asin 2.3)", env))
        assertEquals(BigComplex(BigDecimal("0.6662394324925154"), BigDecimal("1.0612750619050357")), eval("(asin 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("-0.3076036495307112"), BigDecimal("-1.8641615441578825")), eval("(asin -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(asin 99999999999999999999999999999999999999999999999999999999999999)", env));
    }

    @Test
    fun testAcos() {
        assertEquals(1.5707963267948966, eval("(acos 0)", env))
        assertEquals(0.0, eval("(acos 1)", env))
        assertEquals(3.141592653589793, eval("(acos -1)", env))
        assertEquals(0.0, eval("(acos 1/1)", env))
        assertEquals(0.0, eval("(acos 1.0)", env))
        assertEquals(BigComplex(BigDecimal("0"), BigDecimal("1.8115262724608532")), eval("(acos pi)", env))
        assertEquals(BigComplex(BigDecimal("0"), BigDecimal("1.4750447812414251")), eval("(acos 2.3)", env))
        assertEquals(BigComplex(BigDecimal("0.9045568943023813"), BigDecimal("-1.0612750619050357")), eval("(acos 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("1.8783999763256078"), BigDecimal("1.8641615441578825")), eval("(acos -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(acos 99999999999999999999999999999999999999999999999999999999999999)", env));
    }

    @Test
    fun testAtan() {
        assertEquals(0L, eval("(atan 0)", env))
        assertEquals(0.7853981633974483, eval("(atan 1)", env))
        assertEquals(-0.7853981633974483, eval("(atan -1)", env))
        assertEquals(0.7853981633974483, eval("(atan 1/1)", env))
        assertEquals(0.7853981633974483, eval("(atan 1.0)", env))
        assertEquals(1.2626272556789115, eval("(atan pi)", env))
        assertEquals(1.1606689862534056, eval("(atan 2.3)", env))
        assertEquals(BigComplex(BigDecimal("1.0172219678978514"), BigDecimal("0.4023594781085251")), eval("(atan 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("-1.4614618538579256"), BigDecimal("-0.305943857905529")), eval("(atan -1-3i)", env))
        //    assertEquals(-0.1285197485957309, eval("(atan 99999999999999999999999999999999999999999999999999999999999999)", env));
    }
}
