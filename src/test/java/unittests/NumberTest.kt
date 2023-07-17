package unittests

import core.exceptions.ArityException
import core.exceptions.UndefinedIdentifierException
import core.exceptions.WrongTypeException
import core.scm.Complex
import core.scm.Ratio
import org.junit.Assert.*
import org.junit.Test
import java.math.BigDecimal
import java.math.BigInteger

class NumberTest : AbstractTest() {

    @Test
    fun testIdentityElement() {
        assertEquals(0L, eval("(+)"))
        assertEquals(1L, eval("(*)"))
        assertEquals(1L, eval("(lcm)"))
        assertEquals(0L, eval("(gcd)"))
        assertEquals(true, eval("(and)"))
        assertEquals(false, eval("(or)"))
    }

    @Test
    fun testEvalNumbers() {
        assertEquals(1L, eval("1"))
        assertEquals(-15L, eval("-15"))
        assertEquals(-2.5, eval("-2.5"))
        assertEquals(5L, eval("#x5"))
        assertEquals(5L, eval("#X5"))
        assertEquals(15L, eval("#xf"))
        assertEquals(15L, eval("#Xf"))
        assertEquals(13L, eval("#b1101"))
        assertEquals(13L, eval("#B1101"))

        assertEquals(2.0, eval("#b1#"))
        assertEquals(2.0, eval("#B1#"))
        assertEquals(2L, eval("#B10"))
        assertEquals(150.0, eval("15#"))
        assertEquals(10.0, eval("+1#"))
        assertEquals(100.0, eval("#i#d10#"))
        assertEquals(100.0, eval("#d10#"))
        assertEquals(4.0, eval("#b10#"))
        assertEquals(150.0, eval("+15#"))
        assertEquals(100L, eval("#e#d10#"))
        assertEquals(150L, eval("#e15#"))
        assertEquals(Ratio.valueOf("101", "10"), eval("#e#d10.1"))

        assertEquals(Complex(BigDecimal.ONE, BigDecimal(2)), eval("1+2i"))
        assertEquals(Complex(BigDecimal.ONE, BigDecimal(-2)), eval("1-2i"))
        assertEquals(Complex(BigDecimal(-1), BigDecimal(2)), eval("-1+2i"))
        assertEquals(Complex(BigDecimal(-1), BigDecimal(-2)), eval("-1-2i"))
        assertEquals(Complex(BigDecimal.ONE, BigDecimal(2)), eval("#e1+2i"))
        assertEquals(Complex(BigDecimal("1.0"), BigDecimal("2.0")), eval("#i1+2i"))
        assertEquals(Complex(BigDecimal("5"), BigDecimal("29")), eval("#e#b101+11101i"))
        assertEquals(Complex(BigDecimal("5"), BigDecimal("29")), eval("#e#b101+11101i"))
        assertEquals(Complex(BigDecimal("255.0"), BigDecimal("2987.9375")), eval("#x#iFf+BaB.fI"))

        arrayOf("+#", "1e1/2", "1e#", "1e.", "1..", "/1", "+1#1", "__1", "--", "-#.1", "1/2/3").forEach {
            try {
                eval(it)
                fail()
            } catch (e: UndefinedIdentifierException) {
                // expected
            }
        }
    }

    @Test
    fun testEvalMath() {
        assertEquals(null, eval("(+ nil)"))
        assertEquals(null, eval("(- nil)"))
        assertEquals(null, eval("(* nil)"))
        assertEquals(6L,   eval("(+ 1 2 3)"))
        assertEquals(6L,   eval("(+ (byte 1) (int 2) (short 3))"))
        assertEquals(2.1,  eval("(+ (byte 1) (double 1.1)"))
        assertEquals(-2L,  eval("(- 0 2)"))
        assertEquals(-2.0, eval("(- 0 2.0)"))
        assertEquals(BigInteger("-2"), eval("(- 0 (bigint 2))"))
        assertEquals(Ratio.valueOf("-1", "2"), eval("(- 0 1/2)"))
        assertEquals(5.5,  eval("(/ (+ 1 2 3 (- (* 2 2.5 2) 5)) 2)"))
        assertEquals(5.0,  eval("(/ 10.0 2)"))
        assertEquals(Ratio.valueOf("1", "10"), eval("(/ 10)"))
        assertEquals(Ratio.valueOf("13", "4"), eval("(/ 13 4)"))
        assertEquals(2L, eval("(/ 10 5)"))
        assertEquals(2.0, eval("(/ 10.0 5)"))
        assertEquals(2.0, eval("(/ 10 5.0)"))
        assertEquals(BigInteger("18446744073709551614"), eval("(+ Long/MAX_VALUE Long/MAX_VALUE)"))
        assertEquals(BigInteger("18446744073709551615"), eval("(- Long/MAX_VALUE Long/MIN_VALUE)"))
        assertEquals(BigInteger("-85070591730234615856620279821087277056"), eval("(* Long/MAX_VALUE Long/MIN_VALUE)"))
        assertEquals(Ratio.valueOf("9223372036854775807", "-9223372036854775808"), eval("(/ Long/MAX_VALUE Long/MIN_VALUE)"))

        assertEquals(1L, eval("(/ -5 -5)"))
        assertEquals(3L, eval("(/ -15 -5)"))
        assertEquals(Ratio.valueOf("1", "3"), eval("(/ -5 -15)"))

        assertEquals(5L, eval("(abs 5)"))
        assertEquals(5L, eval("(abs -5)"))

        assertEquals(Ratio.valueOf("9999", "3332"), eval("(/ 3332/9999)"))
        assertEquals(Ratio.valueOf("9999", "3332"), eval("(/ 1 3332/9999)"))
        assertEquals(3.0009003601440574, eval("(/ 1.0 3332/9999)"))
        try {
            eval("(+ nil nil)")
            fail()
        } catch (e: NullPointerException) {
            // expected
        }
        // abs
        try {
            eval("(abs)")
            fail()
        } catch (e: ArityException) {
            assertEquals("abs: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.message)
        }
        try {
            eval("(abs 1 2 3)")
            fail()
        } catch (e: ArityException) {
            assertEquals("abs: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 3)", e.message)
        }
        try {
            eval("(abs \"not-a-number\")")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("abs: type mismatch; (expected: Real, given: \"not-a-number\")", e.message)
        }

        // sqrt
        assertEquals(5.0, eval("(sqrt 25)"))
        assertEquals(3.0, eval("(sqrt 9.0)"))
        assertTrue((eval("(sqrt -5)") as Double).isNaN())

        assertEquals(Ratio.valueOf("1", "100"), eval("(/ 1 10 10)"))
        assertEquals(Ratio.valueOf("26", "5"), eval("(+ 1/5 (short 5))"))
        assertEquals(Ratio.valueOf("-71", "12"), eval("(- 1/12 (short 6))"))
        assertEquals(Ratio.valueOf("5", "23"), eval("(* 1/23 (short 5))"))
        assertEquals(Ratio.valueOf("1", "115"), eval("(/ 1/23 (short 5))"))
        assertEquals(9L  , eval("(floor    9999999999999/1000000000000)"))
        assertEquals(-10L, eval("(floor   -9999999999999/1000000000000)"))
        assertEquals(10L,  eval("(ceiling  9999999999999/1000000000000)"))
        assertEquals(-9L,  eval("(ceiling -9999999999999/1000000000000)"))
    }

    @Test
    fun testEvalNumericalComparison() {
        assertEquals(true, eval("(= 1 1 1)"))
        assertEquals(false, eval("(= 1 0 1)"))
        assertEquals(true, eval("(= 0 0.0)"))
        assertEquals(true, eval("(= 0.57 0.5700)"))
        assertEquals(true, eval("(= 7 7.00)"))
        assertEquals(true, eval("(= -234234/234 -234234/234 )"))
        assertEquals(false, eval("(= -134234/234 -234234/234 )"))
        assertEquals(true, eval("(= 2000/3000 2/3)"))
        assertEquals(true, eval("(= -9999999999999999999999999999999.0 -9999999999999999999999999999999.0)"))

        assertEquals(true, eval("(> 2 1)"))
        assertEquals(true, eval("(> 2 1.123)"))
        assertEquals(true, eval("(>= 2 1.123)"))
        assertEquals(true, eval("(>= 2.5 1.123)"))
        assertEquals(true, eval("(<= -2.5 1.123)"))
        assertEquals(true, eval("(< -2.5 1.123)"))
    }

    @Test
    fun testNumberTheoreticDivision() {
        // quotient
        assertEquals(3L,   eval("(quotient 13 4)"))
        assertEquals(3.0,  eval("(quotient 13.0 4)"))
        assertEquals(1L,   eval("(quotient 5 5)"))
        assertEquals(1.0,  eval("(quotient 5.0 5)"))
        assertEquals(1.0,  eval("(quotient -5 -5.0)"))
        assertEquals(-1L,  eval("(quotient -5 5)"))
        assertEquals(-1.0, eval("(quotient -5 5.)"))
        assertEquals(-1.0, eval("(quotient -5 5.)"))
        assertEquals(BigDecimal("78"),  eval("(quotient (new BigDecimal \"234\") (new BigDecimal \"3\"))"))
        assertEquals(BigDecimal("78"),  eval("(quotient (new BigDecimal \"-234\") (new BigDecimal \"-3\"))"))
        assertEquals(BigDecimal("-78"), eval("(quotient (new BigDecimal \"234\") (new BigDecimal \"-3\"))"))
        assertEquals(BigDecimal("-78"), eval("(quotient (new BigDecimal \"-234\") (new BigDecimal \"3\"))"))
        try {
            eval("(quotient -10 0.0001)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("quotient: type mismatch; (expected: Integer, given: 1.0E-4)", e.message)
        }
        try {
            eval("(quotient -10 0.0)")
            fail()
        } catch (e: ArithmeticException) {
            assertEquals("quotient: undefined for 0", e.message)
        }

        // remainder
        assertEquals(-1L, eval("(remainder -13 4)"))
        assertEquals(1L, eval("(remainder 13 -4)"))
        assertEquals(-1L, eval("(remainder -13 -4)"))
        assertEquals(-1.0, eval("(remainder -13 -4.0)"))
        assertEquals(1L, eval("(remainder 13 4)"))
        assertEquals(0L, eval("(remainder 10 2)"))
        assertEquals(0.0, eval("(remainder 10 2.0)"))
        assertEquals(0.0, eval("(remainder -10 2.0)"))
        try {
            eval("(remainder -10 0.0001)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("remainder: type mismatch; (expected: Integer, given: 1.0E-4)", e.message)
        }
        try {
            eval("(remainder -10 0.0)")
            fail()
        } catch (e: ArithmeticException) {
            assertEquals("remainder: undefined for 0", e.message)
        }

        // modulo
        assertEquals(2L, eval("(modulo 5 3)"))
        assertEquals(2.0, eval("(modulo 5 3.0)"))
        assertEquals(1L, eval("(modulo 13 4)"))
        assertEquals(-1L, eval("(modulo -13 -4)"))
        try {
            eval("(modulo -10 0.0001)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("modulo: type mismatch; (expected: Integer, given: 1.0E-4)", e.message)
        }
        try {
            eval("(modulo -10 0.0)")
            fail()
        } catch (e: ArithmeticException) {
            assertEquals("modulo: undefined for 0", e.message)
        }
        assertEquals(3L, eval("(modulo -13 4)"))
        assertEquals(-3L, eval("(modulo 13 -4)"))
    }

    @Test
    fun testEvalIsZero() {
        assertEquals(true,  eval("(zero? 0)"))
        assertEquals(true,  eval("(zero? 0.0)"))
        assertEquals(true,  eval("(zero? 0000000000000000000000000000.00000000000000000000000)"))
        assertEquals(true,  eval("(zero? 0/999999)"))
        assertEquals(true,  eval("(zero? -0/999999)"))
        assertEquals(false, eval("(zero? null)"))
        assertEquals(false, eval("(zero? 1)"))
        assertEquals(false, eval("(zero? -5)"))

        try {
            eval("(zero? \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("zero?: type mismatch; (expected: Number, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalNegative() {
        assertEquals(false, eval("(negative? null)"))
        assertEquals(false, eval("(negative? 0)"))
        assertEquals(false, eval("(negative? 0.0)"))
        assertEquals(false, eval("(negative? 3/4)"))
        assertEquals(false, eval("(negative? 9999999999999999999999999999999999.0)"))
        assertEquals(false, eval("(negative? 1)"))
        assertEquals(false, eval("(negative? (* -5 -6))"))
        assertEquals(false, eval("(negative? (byte 5))"))
        assertEquals(false, eval("(negative? (short 5))"))
        assertEquals(false, eval("(negative? (int 5))"))
        assertEquals(false, eval("(negative? (float 5))"))
        assertEquals(false, eval("(negative? (bigint 5))"))
        assertEquals(true,  eval("(negative? -3/4)"))
        assertEquals(true,  eval("(negative? -9999999999999999999999999999999999.0)"))
        assertEquals(true,  eval("(negative? -5)"))
        assertEquals(true,  eval("(negative? (byte -5))"))
        assertEquals(true,  eval("(negative? (short -5))"))
        assertEquals(true,  eval("(negative? (int -5))"))
        assertEquals(true,  eval("(negative? (float -5))"))
        assertEquals(true,  eval("(negative? (bigint -5))"))
        try {
            eval("(negative? \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("negative?: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalPositive() {
        assertEquals(false, eval("(positive? null)"))
        assertEquals(false, eval("(positive? 0)"))
        assertEquals(false, eval("(positive? 0.0)"))
        assertEquals(false, eval("(positive? -3/4)"))
        assertEquals(false, eval("(positive? -9999999999999999999999999999999999.0)"))
        assertEquals(false, eval("(positive? -5)"))
        assertEquals(false, eval("(positive? (byte -5))"))
        assertEquals(false, eval("(positive? (short -5))"))
        assertEquals(false, eval("(positive? (int -5))"))
        assertEquals(false, eval("(positive? (float -5))"))
        assertEquals(false, eval("(positive? (bigint -5))"))
        assertEquals(true,  eval("(positive? (byte 5))"))
        assertEquals(true,  eval("(positive? (short 5))"))
        assertEquals(true,  eval("(positive? (int 5))"))
        assertEquals(true,  eval("(positive? (float 5))"))
        assertEquals(true,  eval("(positive? (bigint 5))"))
        assertEquals(true,  eval("(positive? 3/4)"))
        assertEquals(true,  eval("(positive? 9999999999999999999999999999999999.0)"))
        assertEquals(true,  eval("(positive? 1)"))
        assertEquals(true,  eval("(positive? (* -5 -6))"))
        try {
            eval("(positive? \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("positive?: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalEven() {
        assertEquals(true, eval("(even? 0)"))
        assertEquals(true, eval("(even? 0.0)"))
        assertEquals(true, eval("(even? 4)"))
        assertEquals(true, eval("(even? 100)"))
        assertEquals(false, eval("(even? 1)"))
        assertEquals(true, eval("(even? (* -5 -6))"))
        assertEquals(false, eval("(even? -5)"))
        try {
            eval("(even? \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("even?: type mismatch; (expected: Integer, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalOdd() {
        assertEquals(false, eval("(odd? 0)"))
        assertEquals(false, eval("(odd? 0.0)"))
        assertEquals(false, eval("(odd? 4)"))
        assertEquals(false, eval("(odd? 100)"))
        assertEquals(true, eval("(odd? 1)"))
        assertEquals(false, eval("(odd? 4)"))
        assertEquals(false, eval("(odd? (* -5 -6))"))
        assertEquals(true, eval("(odd? -5)"))
        try {
            eval("(odd? \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("odd?: type mismatch; (expected: Integer, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalRound() {
        assertEquals(0L, eval("(round 0)"))
        assertEquals(4L, eval("(round 4)"))
        assertEquals(-4L, eval("(round -4)"))
        assertEquals(0.0, eval("(round 0.0)"))
        assertEquals(1.0, eval("(round 1.0)"))
        assertEquals(2.0, eval("(round 1.5)"))
        assertEquals(-2.0, eval("(round -1.5)"))
        assertEquals(2.0, eval("(round 2.5)"))
        assertEquals(-0.0, eval("(round -0.5)"))
        assertEquals(-2.0, eval("(round -1.7)"))
        assertEquals(4.0, eval("(round 3.7)"))
        assertEquals(3.0, eval("(round 2.7)"))
        assertEquals(2.0, eval("(round 2.5)"))
        try {
            eval("(round \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("round: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalFloor() {
        assertEquals(0L, eval("(floor 0)"))
        assertEquals(4L, eval("(floor 4)"))
        assertEquals(-5.0, eval("(floor -4.3)"))
        assertEquals(3.0, eval("(floor 3.5)"))
        assertEquals(1.0, eval("(floor 1.2)"))
        assertEquals(-2.0, eval("(floor -1.2)"))
        try {
            eval("(floor \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("floor: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalCeiling() {
        assertEquals(0L, eval("(ceiling 0)"))
        assertEquals(4L, eval("(ceiling 4)"))
        assertEquals(-4.0, eval("(ceiling -4.3)"))
        assertEquals(4.0, eval("(ceiling 3.5)"))
        assertEquals(2.0, eval("(ceiling 1.2)"))
        assertEquals(-1.0, eval("(ceiling -1.2)"))
        try {
            eval("(ceiling \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("ceiling: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalTruncate() {
        assertEquals(0L, eval("(truncate 0)"))
        assertEquals(4L, eval("(truncate 4)"))
        assertEquals(-4L, eval("(truncate -4)"))
        assertEquals(3.0, eval("(truncate 3.5)"))
        assertEquals(-3.0, eval("(truncate -3.5)"))
        assertEquals(2.0, eval("(truncate 2.2)"))
        assertEquals(-1.0, eval("(truncate -1.2)"))
        try {
            eval("(truncate \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("truncate: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalMax() {
        assertEquals(null, eval("(max nil)"))
        assertEquals(0L,   eval("(max 0)"))
        assertEquals(5.0,  eval("(max 5.0)"))
        assertEquals(-5.0, eval("(max -5.0)"))
        assertEquals(-5.0, eval("(max -6 -7 -5.0)"))
        assertEquals(7L, eval("(max 6 7 5.0)"))
        try {
            eval("(max \"test\" 1 2 3)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("max: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
        try {
            eval("(max 0 \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("max: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalMin() {
        assertEquals(null, eval("(min nil)"))
        assertEquals(0L,   eval("(min 0)"))
        assertEquals(5.0,  eval("(min 5.0)"))
        assertEquals(-5.0, eval("(min -5.0)"))
        assertEquals(-7L,  eval("(min -6 -7 -5.0)"))
        assertEquals(5.0,  eval("(min 6 7 5.0)"))
        try {
            eval("(min \"test\" 1 2 3)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("min: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
        try {
            eval("(min 0 \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("min: type mismatch; (expected: Real, given: \"test\")", e.message)
        }
    }

    @Test
    fun testEvalGCD() {
        // gcd of no args is 0
        assertEquals(0L, eval("(gcd)"))
        // gcd of 0(s) is 0
        assertEquals(0L,  eval("(gcd 0)"))
        assertEquals(0.0, eval("(gcd 0.0)"))
        assertEquals(0L,  eval("(gcd 0 0)"))
        assertEquals(0.0, eval("(gcd 0 0.0)"))
        assertEquals(5L,  eval("(gcd 5 0)"))
        assertEquals(5.0, eval("(gcd 5.0 0)"))
        assertEquals(5L,  eval("(gcd 0 5)"))

        // gcd of n is n
        assertEquals(5L, eval("(gcd 5)"))
        assertEquals(5L, eval("(gcd -5)"))

        // gcd of multiple numbers
        assertEquals(5L, eval("(gcd 5 10)"))
        assertEquals(1L, eval("(gcd 3 6 8)"))

        assertEquals(5L, eval("(gcd  5  5)"))
        assertEquals(5L, eval("(gcd -5  5)"))
        assertEquals(5L, eval("(gcd  5 -5)"))
        assertEquals(5L, eval("(gcd -5 -5)"))

        assertEquals(3.0, eval("(gcd 3.0 6)"))
        assertEquals(40000.0, eval("(gcd 200000.0 40000.0)"))
        assertEquals(BigDecimal("8.8817841970012523233890533447265625E-16"), eval("(gcd 3.3 6)"))
        assertEquals(BigInteger.valueOf(9L),
                     eval("(gcd 999999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                          "99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                          "99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                          "99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                          "99999999999999999999999999999 9)"))

        /* Check switch from Double to BigDecimal for big numbers */
        assertEquals(3L, eval("(gcd 99999999999999999 123)"))
        assertEquals(3L, eval("(gcd 999999999999999999 123)"))
        assertEquals(BigInteger.valueOf(3L), eval("(gcd 9999999999999999999 123)"))
        assertEquals(BigInteger.valueOf(123L), eval("(gcd 99999999999999999999 123)"))
        assertEquals(BigInteger.valueOf(3L), eval("(gcd 999999999999999999999 123)"))
        assertEquals(BigInteger.valueOf(3L), eval("(gcd 9999999999999999999999 123)"))
        assertEquals(BigInteger.valueOf(1L), eval("(gcd (expt 17 34) (expt 21 19))"))
    }

    @Test
    fun testEvalLCM() {
        // lcm of no args is 0
        assertEquals(1L, eval("(lcm)"))
        // lcm of 0(s) is 0
        assertEquals(0L,  eval("(lcm 0)"))
        assertEquals(0.0, eval("(lcm 0.0)"))
        assertEquals(0L,  eval("(lcm 0 0)"))

        // lcm of n is n
        assertEquals(5L,  eval("(lcm 5)"))
        assertEquals(5.0, eval("(lcm 5.0)"))
        assertEquals(5L,  eval("(lcm -5)"))

        // lcm of multiple numbers
        assertEquals(10L,  eval("(lcm 5 10)"))
        assertEquals(24L,  eval("(lcm 3 6 8)"))
        assertEquals(24.0, eval("(lcm 3 6 8.0)"))

        assertEquals(5L, eval("(lcm  5  5)"))
        assertEquals(5L, eval("(lcm -5  5)"))
        assertEquals(5L, eval("(lcm  5 -5)"))
        assertEquals(5L, eval("(lcm -5 -5)"))

        assertEquals(6.0, eval("(lcm 3.0 6)"))
        assertEquals(200000.0, eval("(lcm 200000.0 40000.0)"))
        assertEquals(2.2292818155483952E+16, eval("(lcm 3.3 6)"))

        val big = "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                  "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                  "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                  "99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
        assertEquals(BigInteger(big), eval("(lcm $big 9)"))

        assertEquals(BigInteger("9066296685449845496062520942090242184133343279738102243137866423949"),
                eval("(lcm (expt 17 34) (expt 21 19))"))
        assertEquals(12L, eval("(lcm 1/2 4/5 3/56)"))
    }

    @Test
    fun testEvalExpt() {
        assertEquals(1L, eval("(expt 9 0)"))
        assertEquals(0L, eval("(expt 0 10)"))
        assertEquals(1L, eval("(expt 1 1)"))
        assertEquals(8L, eval("(expt 2 3)"))
        assertEquals(16777216L, eval("(expt 4 12)"))
        assertEquals(25L, eval("(expt -5 2)"))
        assertEquals(-125L, eval("(expt -5 3)"))
        assertEquals(13.489468760533386, eval("(expt 2.2 3.3)"))
        try {
            eval("(expt \"test\" 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("expt: type mismatch; (expected: Number, given: \"test\")", e.message)
        }
        try {
            eval("(expt 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("expt: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 1)", e.message)
        }
        assertEquals(1L, eval("(expt 0 0)"))
        assertEquals(1L, eval("(expt 0.0 0)"))
        assertEquals(1.0, eval("(expt 0 0.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt 0.0 -5)"))
        assertEquals(0.0, eval("(expt 0.0 5)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(expt -0.0 -5)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt -0.0 -6)"))
        assertEquals(-0.0, eval("(expt -0.0 5)"))
        assertEquals(0.0, eval("(expt -0.0 6)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt +inf.0 2)"))
        assertEquals(0.0, eval("(expt +inf.0 -2)"))
        assertEquals(0.0, eval("(expt -inf.0 -2)"))
        assertEquals(-0.0, eval("(expt -inf.0 -3)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt -inf.0 2)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(expt -inf.0 3)"))
        assertEquals(0L, eval("(expt 0 +inf.0)"))
        try {
            eval("(expt 0 -inf.0)")
            fail()
        } catch (e: ArithmeticException) {
            // success
        }

        assertEquals(0.0, eval("(expt 0.5 +inf.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt 5 +inf.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt 0.5 -inf.0)"))
        assertEquals(0.0, eval("(expt 5 -inf.0)"))
        assertEquals(Double.NaN, eval("(expt 2+3i -inf.0)"))
        assertEquals(Double.NaN, eval("(expt 0 +nan.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt 2 2147483648"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(expt -2 2147483647"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt -2 2147483648"))
        assertEquals(9.610000000000001, eval("(expt -3.1 2)"))
        assertEquals(Complex(BigDecimal("-0.0029666184064708"), BigDecimal("0.0009639127514694")),
                eval("(expt -3.1 -5.1)"))
        assertEquals(Complex(BigDecimal("-0.0007509092057438"), BigDecimal("-0.0001938306001686")),
                eval("(expt -3.1 2+3i)"))
        assertEquals(Complex(BigDecimal("-2.4728383E-9"), BigDecimal("9.0752993E-9")),
                eval("(expt -2.3-3.4i -4.5-5.6i)"))
        assertEquals(244140625L, eval("(expt 5 12)"))
        assertEquals(244140625.0, eval("(expt 5.0 12)"))
        assertEquals(244140625.0, eval("(expt (double 5) 12)"))
        assertEquals(244140625.0, eval("(expt (float 5)  12)"))
        assertEquals(244140625L, eval("(expt (short 5)  12)"))
        assertEquals(244140625L, eval("(expt (byte 5)   12)"))
        assertEquals(244140625L, eval("(expt (int 5)    12)"))
        assertEquals(BigInteger.valueOf(244140625L), eval("(expt (bigint 5) 12)"))
        assertEquals(BigDecimal.valueOf(244140625L), eval("(expt (bigdec 5) 12)"))
        assertEquals(BigDecimal("244140625.0"), eval("(expt (bigdec 5.0) 12)"))
        assertEquals(BigDecimal("244140625.0"), eval("(expt (bigdec 5.000) 12)"))
    }

    @Test
    fun testBigDecimal() {

        val big0 = "200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
                   "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" +
                   "00000000000000000000000000000000000000000000000000"

        val big1 = BigInteger("46253220748483971668312221557921994865355615937726643119142534763573384386717010179" +
                              "33035570379376866259256597659610145236756255853523897837700465755231009606767424706" +
                              "73348011864143704094117912896787287887428473700404535710995567015427340013704721113" +
                              "33316343260021388334157118647272603383652782933091786683436259682248241271823609303" +
                              "74088996645418723950501545025386234033486857262740020740808886229945286599837304752" +
                              "25837944037056491016035642078654527374207462210630264065442615967117912099739418879" +
                              "84132513")

        assertEquals(big1, eval("(expt 33 333)"))
        assertEquals(true, eval("(number? $big0)"))
        assertEquals(true, eval("(complex? $big0)"))

        assertEquals(BigInteger(big0), eval("(* (/ $big0 10) 10)"))
        assertEquals(BigInteger(big0).multiply(BigInteger("2")), eval("(+ $big0 $big0)"))
        assertEquals(BigInteger(big0).multiply(BigInteger("2")).subtract(BigInteger(big0)),
                eval("(- (* 2 $big0) $big0)"))

        assertEquals(BigInteger("999999999999997000000000000002999999999999999"),
                eval("(* 999999999999999 999999999999999 999999999999999)"))

        assertEquals(BigDecimal(big0).setScale(1), eval("(truncate (+ 0.2 $big0))"))
        assertEquals(BigDecimal(big0).setScale(1).negate(), eval("(truncate (+ 0.2 -$big0))"))
        assertEquals(BigDecimal(big0).setScale(1), eval("(floor (+ 0.2 $big0))"))
        assertEquals(BigDecimal(big0).setScale(1).add(BigDecimal.ONE), eval("(ceiling (+ 0.2 $big0))"))

        assertEquals(BigInteger(big0), eval("(abs -$big0)"))
        assertEquals(BigInteger(big0).add(BigInteger.ONE), eval("(max (+ 1 $big0) $big0)"))

        assertEquals(BigInteger(big0), eval("(min (+ 1 $big0) $big0)"))

        val big2 = "94173726847307563448129406353133384765848500245816852710163983800558218551747348381698338922873" +
                   "20664371652943772951092101767958590478763994607715301818288618439948015263206590672606004430633" +
                   "76955200810073997787724454002350759571876705644517946943898492214066331998886559185229835330687" +
                   "16557736551944939542436690422291330669696133008408637794606316913830389769724220619283620927344" +
                   "48732510234117642719447040883138454465897687277607911851702661446045370451736296630457393007679" +
                   "85189493967771010336173962367396474652866334212802605674879313278209206179544726008444885447395" +
                   "75799188387594545786910357390161277731611224743862962408171814371026910878890438900816720909115" +
                   "10022168930517460190916457428392515132688370942488090185210467345302536060537534456041560509037" +
                   "37280600427015788467630468023527367174845920094011539693975275654700093627716413"
        assertEquals(BigInteger.valueOf(1L),  eval("(modulo $big2 4)"))
        assertEquals(BigInteger.valueOf(-2L), eval("(modulo $big2 -5)"))
        assertEquals(BigInteger.valueOf(1L),  eval("(remainder $big2 4)"))
        assertEquals(BigInteger.valueOf(3L),  eval("(remainder $big2 -5)"))

        val quotientResult1 = "470868634236537817240647031765666923829242501229084263550819919002791092758736741908" +
                              "491694614366033218582647188647554605088397929523938199730385765090914430921997400763" +
                              "160329533630300221531688477600405036998893862227001175379785938352822258973471949246" +
                              "107033165999443279592614917665343582788682759724697712183452111456653348480665042043" +
                              "188973031584569151948848621103096418104636722436625511705882135972352044156922723294" +
                              "884363880395592585133072302268522586814831522869650383992594746983885505168086981183" +
                              "698237326433167106401302837439656639104603089772363004222442723697878995941937972728" +
                              "934551786950806388658056123719314812040859071855134554394452194504083604545575501108" +
                              "446525873009545822871419625756634418547124404509260523367265126803026876722802078025" +
                              "451868640300213507894233815234011763683587422960047005769846987637827350046813858206"
        assertEquals(BigInteger(quotientResult1), eval("(quotient $big2 2)"))
        assertEquals(BigInteger.valueOf(2L), eval("(quotient $big2 (quotient $big2 2))"))

        assertEquals(true,  eval("(eqv? $big2 $big2)"))
        assertEquals(true,  eval("(<=   $big2 $big2)"))
        assertEquals(false, eval("(<    $big2 $big2)"))
        assertEquals(true,  eval("(> (+ 1 $big2) $big2)"))
        assertEquals(true,  eval("(< (+ 1 2) $big2)"))

        assertEquals(Double.POSITIVE_INFINITY, eval("(sqrt $big2)"))
        assertEquals(BigInteger("-99999999999999999999999999999999999999999999999999"),
                     eval("(- 99999999999999999999999999999999999999999999999999)"))
        assertEquals(1e247, eval("(+ 1/23 99999999999999999999999999999999999999999999999999999999999999999999999999" +
                                 "9999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                                 "9999999999999999999999999999999999999999999999999999999999999999999999999999999999" +
                                 "999999999.1)"))
    }

    @Test
    fun testEvalIsInteger() {
        assertEquals(true,  eval("(integer? 0)"))
        assertEquals(true,  eval("(integer? 0.0)"))
        assertEquals(true,  eval("(integer? 4)"))
        assertEquals(true,  eval("(integer? 100)"))
        assertEquals(true,  eval("(integer? 1)"))
        assertEquals(true,  eval("(integer? (* -5 -6))"))
        assertEquals(true,  eval("(integer? -5)"))
        assertEquals(false, eval("(integer? null)"))
        assertEquals(false, eval("(integer? -5.4)"))
        assertEquals(false, eval("(integer? 3.14)"))
        assertEquals(false, eval("(integer? .123)"))
        assertEquals(false, eval("(integer? \"test\")"))
    }

    @Test
    fun testNumberToString() {
        assertEquals("5", eval("(number->string 5)"))
        assertEquals("5.5", eval("(number->string 5.5)"))
        assertEquals("9999999999999999999999999999999", eval("(number->string #d9999999999999999999999999999999)"))
        assertEquals("9999999999999999999999999999999.5", eval("(number->string 9999999999999999999999999999999.5)"))

        assertEquals("5", eval("(number->string #b101)"))
        assertEquals("309461373397964671249896789",
                     eval("(number->string #b1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101)"))
        assertEquals("309461373397964671249896789",
                     eval("(number->string #B1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101)"))

        assertEquals("449", eval("(number->string #o701)"))
        assertEquals("29889", eval("(number->string #o72301)"))
        assertEquals("1237940039285380274899121345", eval("(number->string #o777777777777777777777777772301)"))
        assertEquals("1237940039285380274899121345", eval("(number->string #O777777777777777777777777772301)"))

        assertEquals("0", eval("(number->string #x0)"))
        assertEquals("15", eval("(number->string #xf)"))
        assertEquals("255", eval("(number->string #xff)"))
        assertEquals("324518553658426726783156020576255", eval("(number->string #xfffffffffffffffffffffffffff)"))

        assertEquals("777777777777777777777777777777777777", eval("(number->string #xfffffffffffffffffffffffffff 8)"))
        assertEquals("11111111", eval("(number->string #xff 2)"))
        assertEquals("1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101",
                     eval("(number->string #b1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101 2)"))
        assertEquals("3a885", eval("(number->string 239749 16)"))

        assertEquals("-3a885", eval("(number->string -239749 16)"))
        assertEquals("-777777777777777777777777777777777777", eval("(number->string #x-fffffffffffffffffffffffffff 8)"))

        "123456789abcdefghijklmnopqrstuvwxyz".forEachIndexed { i, c ->
            assertEquals("$c", eval("(number->string (string->number \"$c\" ${i + 2}) ${i + 2})"))
        }
        "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".forEachIndexed { i, c ->
            assertEquals("$c".toLowerCase(), eval("(number->string (string->number \"$c\" ${i + 2}) ${i + 2})"))
        }
    }

    @Test
    fun testStringToNumber() {
        assertEquals(100L, eval("(string->number \"100\")"))
        assertEquals(256L, eval("(string->number \"100\" 16)"))
        assertEquals(false, eval("(string->number \"hello\")"))
        assertEquals(57L, eval("(string->number \"111\" 7)"))
        assertEquals(7L, eval("(string->number \"#b111\" 7)"))
        assertEquals(BigInteger("26623333280885243903"), eval("(string->number \"bbbbbbbbbbbbbbbbbb\" 12)"))
        assertEquals(BigInteger("26623333280885243903"), eval("(string->number \"BBBBBBBBBBBBBBBBBB\" 12)"))
        assertEquals(BigInteger("110573323209400121422731899656355381011962890624"), eval("(string->number \"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\" 15)"))
        assertEquals(BigDecimal("110573323209400121422731899656355381011962890624.0"), eval("(string->number \"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee.0\" 15)"))
        assertEquals(false, eval("(string->number \"eeef\" 15)"))
        assertEquals(85L, eval("(string->number \"1010101\" 2)"))
        assertEquals(BigInteger("289264344747772786367397236066475587972918828808734345141483382767615"),
                     eval("(string->number \"#xababaabababababababababababababafffffffffffffffffffffffff\")"))
        assertEquals(BigDecimal("289264344747772786367397236066475587972918828808734345141483382767615.0"),
                     eval("(string->number \"#xababaabababababababababababababafffffffffffffffffffffffff.0\")"))
        assertEquals(1500.0, eval("(string->number \"15##\")"))
        assertEquals(false, eval("(string->number \"1234#d\")"))
        assertEquals(100.0, eval("(string->number \"1e2\")"))
        assertEquals(BigDecimal("0.5"), eval("(string->number \"#b1e-1\")"))
        assertEquals(BigDecimal("6161212520618990239744.0"), eval("(string->number \"#o1234e+25\")"))
        assertEquals(false, eval("(string->number \"#\")"))
        assertEquals(false, eval("(string->number \"#e\")"))
        assertEquals(false, eval("(string->number \"##\")"))
        assertEquals(false, eval("(string->number \"#e#\")"))
        assertEquals(false, eval("(string->number \"##e\")"))
        assertEquals(false, eval("(string->number \"###\")"))
        assertEquals(false, eval("(string->number \"###e\")"))
        assertEquals(false, eval("(string->number \"#e##\")"))
        assertEquals(false, eval("(string->number \"#e#e\")"))
        assertEquals(false, eval("(string->number \"eeef\")"))
        assertEquals(false, eval("(string->number \"#e#i1\")"))
        assertEquals(false, eval("(string->number \"#e#b#\")"))
        assertEquals(false, eval("(string->number \"#e#i#e\")"))
        assertEquals(false, eval("(string->number \"1.1.1\")"))
        assertEquals(false, eval("(string->number \"#e#b-1010/101011e4\")"))
        assertEquals(false, eval("(string->number \"#b1e5\")"))
    }

    @Test
    fun testSpecialNumbers() {
        assertEquals(Double.NaN, eval("+nan.0"))
        assertEquals(Double.NaN, eval("-nan.0"))
        assertEquals(Double.POSITIVE_INFINITY, eval("+inf.0"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("-inf.0"))
    }

    @Test
    fun testThresholds() {
        val numbers = hashMapOf(2  to "1111111111111111111111111111111111111111111111111111111111111111",
                                3  to "2222222222222222222222222222222222222222",
                                4  to "33333333333333333333333333333333",
                                5  to "4444444444444444444444444444",
                                6  to "5555555555555555555555555",
                                7  to "66666666666666666666666",
                                8  to "7777777777777777777777",
                                9  to "88888888888888888888",
                                10 to "9999999999999999999",
                                11 to "aaaaaaaaaaaaaaaaaaa",
                                12 to "bbbbbbbbbbbbbbbbbb",
                                13 to "cccccccccccccccccc",
                                14 to "ddddddddddddddddd",
                                15 to "eeeeeeeeeeeeeeeee",
                                16 to "ffffffffffffffff",
                                17 to "gggggggggggggggg",
                                18 to "hhhhhhhhhhhhhhhh",
                                19 to "iiiiiiiiiiiiiii",
                                20 to "jjjjjjjjjjjjjjj",
                                21 to "kkkkkkkkkkkkkkk",
                                22 to "lllllllllllllll",
                                23 to "mmmmmmmmmmmmmm",
                                24 to "nnnnnnnnnnnnnn",
                                25 to "oooooooooooooo",
                                26 to "pppppppppppppp",
                                27 to "qqqqqqqqqqqqqq",
                                28 to "rrrrrrrrrrrrrr",
                                29 to "sssssssssssss",
                                30 to "ttttttttttttt",
                                31 to "uuuuuuuuuuuuu",
                                32 to "vvvvvvvvvvvvv",
                                33 to "wwwwwwwwwwwww",
                                34 to "xxxxxxxxxxxxx",
                                35 to "yyyyyyyyyyyyy",
                                36 to "zzzzzzzzzzzzz")
        numbers.forEach { (k, v) -> assertNotEquals(false, eval("(string->number \"$v\" $k)")) }
    }

    @Test
    fun testQuotientViaTruncate() {
        eval("(define // (lambda (n m) (truncate (/ n m))))")
        assertEquals(true, eval("(= (quotient 5 4) (// 5 4))"))
        assertEquals(true, eval("(= (quotient 5 4.0) (// 5 4.0))"))
        assertEquals(true, eval("(= (quotient -5 4.0) (// -5 4.0))"))
        assertEquals(eval("(quotient -5999999999999999999999999999999999999 4.0)"),
                     eval("(// -5999999999999999999999999999999999999 4.0)"))
    }

    @Test
    fun testIsRational() {
        assertEquals(true, eval("(rational? 0)"))
        assertEquals(true, eval("(rational? 1.0)"))
        assertEquals(true, eval("(rational? -234234/234234)"))
        assertEquals(true, eval("(rational? -83457348957348573498573489573489573489583457389457349534.3489534895)"))

        assertEquals(false, eval("(rational? +inf.0)"))
        assertEquals(false, eval("(rational? -inf.0)"))
        assertEquals(false, eval("(rational? +nan.0)"))
        assertEquals(false, eval("(rational? -nan.0)"))
        assertEquals(false, eval("(rational? \"test\")"))
        assertEquals(false, eval("(rational? 12-4i)"))
    }

    @Test
    fun testIsReal() {
        assertEquals(true, eval("(real? 0)"))
        assertEquals(true, eval("(real? 1.0)"))
        assertEquals(true, eval("(real? -234234/234234)"))
        assertEquals(true, eval("(real? -83457348957348573498573489573489573489583457389457349534.3489534895)"))
        assertEquals(true, eval("(real? +inf.0)"))
        assertEquals(true, eval("(real? -inf.0)"))
        assertEquals(true, eval("(real? +nan.0)"))
        assertEquals(true, eval("(real? -nan.0)"))
        assertEquals(false, eval("(real? \"test\")"))
    }

    @Test
    fun testIsNumber() {
        assertEquals(true, eval("(number? 0)"))
        assertEquals(true, eval("(number? 1.0)"))
        assertEquals(true, eval("(number? -234234/234234)"))
        assertEquals(true, eval("(number? -83457348957348573498573489573489573489583457389457349534.3489534895)"))
        assertEquals(true, eval("(number? +inf.0)"))
        assertEquals(true, eval("(number? -inf.0)"))
        assertEquals(true, eval("(number? +nan.0)"))
        assertEquals(true, eval("(number? -nan.0)"))
        assertEquals(true, eval("(number? -1/3)"))
        assertEquals(false, eval("(number? \"test\")"))
    }

    @Test
    fun testIsComplex() {
        assertEquals(true, eval("(complex? 0)"))
        assertEquals(true, eval("(complex? 1.0)"))
        assertEquals(true, eval("(complex? -234234/234234)"))
        assertEquals(true, eval("(complex? -83457348957348573498573489573489573489583457389457349534.3489534895)"))
        assertEquals(true, eval("(complex? +inf.0)"))
        assertEquals(true, eval("(complex? -inf.0)"))
        assertEquals(true, eval("(complex? +nan.0)"))
        assertEquals(true, eval("(complex? -nan.0)"))
        assertEquals(true, eval("(complex? -1/3)"))
        assertEquals(false, eval("(complex? \"test\")"))
    }

    @Test
    fun testIsExact() {
        assertEquals(true,  eval("(exact? 0)"))
        assertEquals(true,  eval("(exact? -3/5)"))
        assertEquals(true,  eval("(exact? 99999999999999999999999999999999999999999999999999999999999999999999)"))
        assertEquals(false, eval("(exact? null)"))
        assertEquals(false, eval("(exact? -2.5)"))
        assertEquals(false, eval("(exact? +inf.0)"))
        assertEquals(false, eval("(exact? -inf.0)"))
        assertEquals(false, eval("(exact? +nan.0)"))
        assertEquals(false, eval("(exact? -nan.0)"))

        try {
            eval("(exact? \"test\")")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("exact?: type mismatch; (expected: Number, given: \"test\")", e.message)
        }
    }

    @Test
    fun testIsInexact() {
        assertEquals(false, eval("(inexact? 0)"))
        assertEquals(false, eval("(inexact? -3/5)"))
        assertEquals(false, eval("(inexact? 99999999999999999999999999999999999999999999999999999999999999999999)"))

        val trues = arrayOf("(inexact? -2.5)", "(inexact? +inf.0)", "(inexact? -inf.0)", "(inexact? +nan.0)", "(inexact? -nan.0)")
        assertAllEqual(true, trues)
        try {
            eval("(inexact? \"test\")")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("inexact?: type mismatch; (expected: Number, given: \"test\")", e.message)
        }
    }

    @Test
    fun testNumerator() {
        assertEquals(1L, eval("(numerator 1)"))
        assertEquals(-1234L, eval("(numerator -1234)"))
        assertEquals(-1234.0, eval("(numerator -1234.0)"))
        assertEquals(BigInteger.valueOf(17L), eval("(numerator 17/4)"))
        assertEquals(BigDecimal("2589569785738035.0"), eval("(numerator 2.3)"))
    }

    @Test
    fun testDenominator() {
        assertEquals(1L, eval("(denominator 1)"))
        assertEquals(1L, eval("(denominator -1234)"))
        assertEquals(1.0, eval("(denominator -1234.0)"))
        assertEquals(BigInteger.valueOf(4L), eval("(denominator 17/4)"))
        assertEquals(BigDecimal("1125899906842624.0"), eval("(denominator 2.3)"))
        assertEquals(Ratio.valueOf("9347593487539475934753495739845734957349857349573495873459374589347593475394857393453454353", "10000000000"),
                     eval("(inexact->exact 934759348753947593475349573984573495734985734957349587345937458934759347539485739.3453454353)"))
    }

    @Test
    fun testExp() {
        assertEquals(1L, eval("(exp 0)"))
        assertEquals(2.718281828459045, eval("(exp 1)"))
        assertEquals(59874.14171519782, eval("(exp 11)"))
        assertEquals(0.36787944117144233, eval("(exp -1)"))
        assertEquals(2.117000016612675, eval("(exp 3/4)"))
        assertEquals(2.718281828459045, eval("(exp 1/1)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(exp 999999999)"))
        assertEquals(0.0, eval("(exp -999999999)"))
    }

    @Test
    fun testLog() {
        assertEquals(0L, eval("(log 1)"))
        assertEquals(2.3978952727983707, eval("(log 11)"))
        assertEquals(Double.NaN, eval("(log -1)"))
        assertEquals(-0.2876820724517809, eval("(log 3/4)"))
        assertEquals(0L, eval("(log 1/1)"))
        assertEquals(20.72326583594641, eval("(log 999999999)"))
        assertEquals(Double.NaN, eval("(log -999999999)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(log 0.0)"))
        assertEquals(110.52408446371419, eval("(log 999999999999999999999999999999999999999999999999)"))
        assertEquals(135.8525204866487, eval("(log 99999999999999999999999999999999999999999999999999999999999)"))
        assertEquals(13562.368703607815, eval("(log (expt 3 12345))"))
        try {
            assertEquals(1L, eval("(log 0)"))
            fail()
        } catch (e: ArithmeticException) {
            assertEquals("log: undefined for 0", e.message)
        }
        try {
            assertEquals(1L, eval("(log 0/1)"))
            fail()
        } catch (e: ArithmeticException) {
            assertEquals("log: undefined for 0", e.message)
        }
    }

    @Test
    fun testFindBetween() {
        val findBetween = "(define (find-between lo hi)" +
                "  (if (integer? lo)" +
                "      lo" +
                "    (let ((lo-int (floor lo))" +
                "          (hi-int (floor hi)))" +
                "      (if (< lo-int hi-int)" +
                "          (+ 1 lo-int)" +
                "        (+ lo-int" +
                "           (/ (find-between (/ (- hi lo-int)) (/ (- lo lo-int)))))))))"
        eval(findBetween)
        assertEquals(Ratio.valueOf("1", "3"), eval("(find-between 3332/9999 3334/9999)"))
    }

    @Test
    fun testRationalize() {
        assertEquals(0L, eval("(rationalize 1/3 1/3)"))
        assertEquals(Ratio.valueOf("1", "3"), eval("(rationalize 1/3 1/9999)"))
        assertEquals(0L, eval("(rationalize 2/3 1)"))
        assertEquals(2333L, eval("(rationalize 2335 2)"))
        assertEquals(-2L, eval("(rationalize -5 3)"))
    }

    @Test
    fun testArithmeticWIthNonFiniteNumbers() {
        assertEquals(Double.NaN, eval("(+ +nan.0 1)"))
        assertEquals(Double.NaN, eval("(+ -nan.0 1)"))
        assertEquals(Double.NaN, eval("(+ 1 +nan.0)"))
        assertEquals(Double.NaN, eval("(+ 1 -nan.0)"))
        assertEquals(Double.NaN, eval("(- +nan.0 1)"))
        assertEquals(Double.NaN, eval("(- -nan.0 1)"))
        assertEquals(Double.NaN, eval("(- 1 +nan.0)"))
        assertEquals(Double.NaN, eval("(- 1 -nan.0)"))
        assertEquals(Double.NaN, eval("(* +nan.0 1)"))
        assertEquals(Double.NaN, eval("(* -nan.0 1)"))
        assertEquals(Double.NaN, eval("(* 1 +nan.0)"))
        assertEquals(Double.NaN, eval("(* 1 -nan.0)"))
        assertEquals(Double.NaN, eval("(/ +nan.0 1)"))
        assertEquals(Double.NaN, eval("(/ -nan.0 1)"))
        assertEquals(Double.NaN, eval("(/ 1 +nan.0)"))
        assertEquals(Double.NaN, eval("(/ 1 -nan.0)"))

        assertEquals(Double.POSITIVE_INFINITY, eval("(+ +inf.0 1)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(+ 1 +inf.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(- +inf.0 1)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(- 1 +inf.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(* +inf.0 1)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(* 1 +inf.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(/ +inf.0 1)"))
        assertEquals(0.0, eval("(/ 1 +inf.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(expt 0.0 -inf.0)"))
        assertEquals(Double.POSITIVE_INFINITY, eval("(- -inf.0)"))

        assertEquals(Double.NEGATIVE_INFINITY, eval("(+ -inf.0 1)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(+ 1 -inf.0)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(- -inf.0 1)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(- 1 -inf.0)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(* -inf.0 1)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(* 1 -inf.0)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(/ -inf.0 1)"))
        assertEquals(-0.0, eval("(/ 1 -inf.0)"))
        assertEquals(Double.NEGATIVE_INFINITY, eval("(- +inf.0)"))

        assertEquals(0.0, eval("(* +nan.0 0)"))
        assertEquals(0.0, eval("(* 0 +nan.0)"))
        assertEquals(0.0, eval("(* -nan.0 0)"))
        assertEquals(0.0, eval("(* 0 -nan.0)"))
        assertEquals(0.0, eval("(* +inf.0 0)"))
        assertEquals(0.0, eval("(* 0 +inf.0)"))
        assertEquals(0.0, eval("(* -inf.0 0)"))
        assertEquals(0.0, eval("(* 0 -inf.0)"))

        assertEquals(0.0,  eval("(/ 0 +nan.0)"))
        assertEquals(0.0,  eval("(/ 0 -nan.0)"))
        assertEquals(0.0,  eval("(/ 0 +inf.0)"))
        assertEquals(-0.0, eval("(/ 0 -inf.0)"))

        assertEquals(Double.NaN, eval("(/ 0.0 0.0)"))
        assertEquals(Double.NaN, eval("(* 0.0 +nan.0)"))
        assertEquals(Double.NaN, eval("(* 0.0 -nan.0)"))
        assertEquals(Double.NaN, eval("(* 0.0 +inf.0)"))
        assertEquals(Double.NaN, eval("(* 0.0 -inf.0)"))
        assertEquals(Double.NaN, eval("(* +nan.0 0.0)"))
        assertEquals(Double.NaN, eval("(* -nan.0 0.0)"))
        assertEquals(Double.NaN, eval("(* +inf.0 0.0)"))
        assertEquals(Double.NaN, eval("(* -inf.0 0.0)"))

        assertEquals(Double.NaN, eval("(+ -inf.0 +inf.0)"))
        assertEquals(Double.NaN, eval("(+ +inf.0 -inf.0)"))
        assertEquals(Double.NaN, eval("(- -inf.0 +inf.0)"))
        assertEquals(Double.NaN, eval("(- +inf.0 -inf.0)"))

        val divByZero = arrayOf("(/ +nan.0 0)", "(/ -nan.0 0)", "(/ +inf.0 0)", "(/ -inf.0 0)")
        for (s in divByZero) {
            try {
                eval(s)
                fail()
            } catch (e: ArithmeticException) {
                // expected
            }
        }
    }

    @Test
    fun testIsNan() {
        assertEquals(true,  eval("(nan? +nan.0)"))
        assertEquals(true,  eval("(nan? -nan.0)"))
        assertEquals(false, eval("(nan? +inf.0)"))
        assertEquals(false, eval("(nan? 0)"))
        try {
            eval("(nan? 'a)")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testIsFinite() {
        assertEquals(true,  eval("(finite? 0)"))
        assertEquals(false, eval("(finite? +inf.0)"))
        assertEquals(false, eval("(finite? -inf.0)"))
        assertEquals(false, eval("(finite? -nan.0)"))
        try {
            eval("(finite? 1+2i)")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
        try {
            eval("(finite? 'a)")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testIsInfinite() {
        assertEquals(false, eval("(infinite? 0)"))
        assertEquals(true,  eval("(infinite? +inf.0)"))
        assertEquals(true,  eval("(infinite? -inf.0)"))
        assertEquals(false, eval("(infinite? -nan.0)"))
        try {
            eval("(infinite? 1+2i)")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
        try {
            eval("(infinite? 'a)")
            fail()
        } catch (e: WrongTypeException) {
            // expected
        }
    }

    @Test
    fun testIsNatural() {
        assertEquals(true,  eval("(exact-nonnegative-integer?  1)"))
        assertEquals(false, eval("(exact-nonnegative-integer? -1)"))
        assertEquals(false, eval("(exact-nonnegative-integer? 'a)"))
        assertEquals(true,  eval("(natural?  1)"))
        assertEquals(false, eval("(natural? -1)"))
        assertEquals(false, eval("(natural? 'a)"))
    }

    @Test
    fun testIsPositiveInteger() {
        assertEquals(true,  eval("(positive-integer?  1)"))
        assertEquals(true,  eval("(positive-integer?  1.0)"))
        assertEquals(false, eval("(positive-integer?  1.3)"))
        assertEquals(false, eval("(positive-integer?  0)"))
        assertEquals(false, eval("(positive-integer? -1)"))
        assertEquals(false, eval("(positive-integer?  1+2i)"))
        assertEquals(false, eval("(positive-integer? 'a)"))
    }

    @Test
    fun testIsNegativeInteger() {
        assertEquals(true,  eval("(negative-integer? -1)"))
        assertEquals(true,  eval("(negative-integer? -1.0)"))
        assertEquals(false, eval("(negative-integer? 1.3)"))
        assertEquals(false, eval("(negative-integer? 0)"))
        assertEquals(false, eval("(negative-integer? 1)"))
        assertEquals(false, eval("(negative-integer? 1+2i)"))
        assertEquals(false, eval("(negative-integer? 'a)"))
    }

    @Test
    fun testIsNonPositiveInteger() {
        assertEquals(true,  eval("(nonpositive-integer? -1)"))
        assertEquals(true,  eval("(nonpositive-integer? -1.0)"))
        assertEquals(true,  eval("(nonpositive-integer? 0)"))
        assertEquals(true,  eval("(nonpositive-integer? 0.0)"))
        assertEquals(false, eval("(nonpositive-integer? 1.3)"))
        assertEquals(false, eval("(nonpositive-integer? 1)"))
        assertEquals(false, eval("(nonpositive-integer? 1+2i)"))
        assertEquals(false, eval("(nonpositive-integer? 'a)"))
    }

    @Test
    fun testIsNonNegativeInteger() {
        assertEquals(true,  eval("(nonnegative-integer?  1)"))
        assertEquals(true,  eval("(nonnegative-integer?  1.0)"))
        assertEquals(true,  eval("(nonnegative-integer?  0)"))
        assertEquals(true,  eval("(nonnegative-integer?  0.0)"))
        assertEquals(false, eval("(nonnegative-integer?  1.3)"))
        assertEquals(false, eval("(nonnegative-integer?  -1)"))
        assertEquals(false, eval("(nonnegative-integer?  1+2i)"))
        assertEquals(false, eval("(nonnegative-integer? 'a)"))
    }
}
