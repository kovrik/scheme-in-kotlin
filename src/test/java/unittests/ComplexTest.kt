package unittests

import core.scm.BigComplex
import core.scm.BigRatio
import org.junit.Test

import java.math.BigDecimal

import java.lang.Boolean.FALSE
import java.lang.Boolean.TRUE
import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class ComplexTest : AbstractTest() {

    @Test
    fun testAddition() {
        assertEquals(BigComplex(BigDecimal("4.75"), BigDecimal("2.0")), eval("(+ 1 3/4 2.5 -2.5+6i 3-4i)", env))
        assertEquals(0.0, eval("(+ 0 0/4 0.0 -000+0i 0-0i)", env))
        assertEquals(BigComplex(BigDecimal("0"), BigDecimal("0")), eval("(+ 1+1i -1-1i)", env))
    }

    @Test
    fun testSubtraction() {
        assertEquals(BigComplex(BigDecimal("-2.75"), BigDecimal("-2.0")), eval("(- 1 3/4 2.5 -2.5+6i 3-4i)", env))
        assertEquals(0.0, eval("(- 0 0/4 0.0 -000+0i 0-0i)", env))
        assertEquals(BigComplex(BigDecimal("2"), BigDecimal("2")), eval("(- 1+1i -1-1i)", env))
    }

    @Test
    fun testMultiplication() {
        assertEquals(BigComplex(BigDecimal("30.9375"), BigDecimal("52.5")), eval("(* 1 3/4 2.5 -2.5+6i 3-4i)", env))
        assertEquals(0.0, eval("(* 0 0/4 0.0 -000+0i 0-0i)", env))
        assertEquals(BigComplex(BigDecimal("0"), BigDecimal("-2")), eval("(* 1+1i -1-1i)", env))
    }

    @Test
    fun testDivision() {
        assertEquals(BigComplex(BigDecimal("0.0083313609467456"), BigDecimal("-0.0141380670611440")),
                eval("(/ 1 3/4 2.5 -2.5+6i 3-4i)", env))

        assertEquals(BigComplex(BigDecimal("-1"), BigDecimal("0")), eval("(/ 1+1i -1-1i)", env))
    }

    @Test
    fun testExactness() {
        assertEquals(TRUE, eval("(exact? 1+2i)", env))
        assertEquals(TRUE, eval("(exact? 0+2i)", env))
        assertEquals(FALSE, eval("(exact? 0+2.0i)", env))
        assertEquals(FALSE, eval("(exact? 2.3+2.4i)", env))
        assertEquals(FALSE, eval("(exact? 0.0+1i)", env))
        assertEquals(FALSE, eval("(inexact? 1+2i)", env))
        assertEquals(FALSE, eval("(inexact? 0+2i)", env))
        assertEquals(TRUE, eval("(inexact? 0+2.0i)", env))
        assertEquals(TRUE, eval("(inexact? 0.0+1i)", env))
        assertEquals(FALSE, eval("(exact? (exact->inexact 1+2i))", env))
        // FIXME Rationals
        //    assertEquals(TRUE,  eval("(exact? (inexact->exact 1.3+2.4i))", env));
    }

    @Test
    fun testSqrt() {
        assertEquals(BigComplex(BigDecimal("1.09868411346781"), BigDecimal("0.4550898605622274")), eval("(sqrt 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("0.7071067811865476"), BigDecimal("-2.1213203435596424")), eval("(sqrt -4-3i)", env))
    }

    @Test
    fun testLog() {
        assertEquals(BigComplex(BigDecimal("0.3465735902799727"), BigDecimal("0.7853981633974483")), eval("(log  1+1i)", env))
        assertEquals(BigComplex(BigDecimal("1.6094379124341003"), BigDecimal("-2.498091544796509")), eval("(log -4-3i)", env))
        assertEquals(BigComplex(BigDecimal("1.6094379124341003"), BigDecimal("-0.6435011087932844")), eval("(log  4-3i)", env))
        assertEquals(BigComplex(BigDecimal("1.6094379124341003"), BigDecimal("0.6435011087932844")), eval("(log  4+3i)", env))
        assertEquals(BigComplex(BigDecimal("1.6094379124341003"), BigDecimal("2.498091544796509")), eval("(log -4+3i)", env))
    }

    @Test
    fun testExpt() {
        assertEquals(BigComplex(BigDecimal("0.2739572538301211"), BigDecimal("0.5837007587586147")), eval("(expt 1+1i 1+1i)", env))
        assertEquals(BigComplex(BigDecimal("0.0004911725350693"), BigDecimal("-0.0007294124825312")), eval("(expt 7+2.3i -4-3i)", env))
        assertEquals(BigComplex(BigDecimal("-345.3968959025678"), BigDecimal("11.028099235573535")), eval("(expt 3.4-5.2i 3.2)", env))
    }

    @Test
    fun testRealPart() {
        assertEquals(BigDecimal.valueOf(1L), eval("(real-part 1+1i)", env))
        assertEquals(BigDecimal("-34.1"), eval("(real-part -34.1+1i)", env))
        assertEquals(BigDecimal("0.1"), eval("(real-part 0.1+1i)", env))
        assertEquals(BigDecimal.valueOf(0L), eval("(real-part 0+1i)", env))
        assertEquals(1L, eval("(real-part 1)", env))
        assertEquals(-2.5, eval("(real-part -2.5)", env))
        assertEquals(BigRatio.valueOf("3", "4"), eval("(real-part 3/4)", env))
    }

    @Test
    fun testImagPart() {
        assertEquals(BigDecimal.valueOf(1L), eval("(imag-part 1+1i)", env))
        assertEquals(BigDecimal.valueOf(1.0), eval("(imag-part -34.1+1i)", env))
        assertEquals(BigDecimal("0.1"), eval("(imag-part 0.1+0.1i)", env))
        assertEquals(BigDecimal("-1.43"), eval("(imag-part 0-1.43i)", env))
        assertEquals(0L, eval("(imag-part 1)", env))
        assertEquals(0L, eval("(imag-part -2.5)", env))
        assertEquals(0L, eval("(imag-part 3/4)", env))
    }

    @Test
    fun testMakePolar() {
        assertEquals(BigComplex(BigDecimal("-1.960930862590836"), BigDecimal("-2.2704074859237846")), eval("(make-polar 3 4)", env))
        assertEquals(BigComplex(BigDecimal.ZERO, BigDecimal.ZERO), eval("(make-polar 0 0)", env))
        assertEquals(BigComplex(BigDecimal("-0.8775825618903728"), BigDecimal("-0.479425538604203")), eval("(make-polar -1 0.5)", env))
    }

    @Test
    fun testMakeRectangular() {
        assertEquals(BigComplex(BigDecimal("3"), BigDecimal("4")), eval("(make-rectangular 3 4)", env))
        assertEquals(BigComplex(BigDecimal.ZERO, BigDecimal.ZERO), eval("(make-rectangular 0 0)", env))
        assertEquals(BigComplex(BigDecimal("-1"), BigDecimal("0.5")), eval("(make-rectangular -1 0.5)", env))
    }

    @Test
    fun testAngle() {
        assertEquals(0.7853981633974483, eval("(angle 1+1i)", env))
        assertEquals(-1.845764097871735, eval("(angle -3.47-12.3i)", env))
        assertEquals(1.5707963267948966, eval("(angle 0+5i)", env))
        assertEquals(0.0, eval("(angle 4)", env))
        assertEquals(3.141592653589793, eval("(angle -1)", env))
        try {
            eval("(angle 0+0i)", env)
            fail()
        } catch (e: ArithmeticException) {
            assertEquals("angle: undefined for 0", e.message)
        }

    }

    @Test
    fun testMagnitude() {
        assertEquals(1.4142135623730951, eval("(magnitude 1+1i)", env))
        assertEquals(12.780097808702404, eval("(magnitude -3.47-12.3i)", env))
        assertEquals(5.0, eval("(magnitude 0+5i)", env))
        assertEquals(4L, eval("(magnitude 4)", env))
        assertEquals(1L, eval("(magnitude -1)", env))
        assertEquals(0L, eval("(magnitude 0+0i)", env))
    }
}
