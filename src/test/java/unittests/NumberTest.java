package unittests;

import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.UndefinedIdentifierException;
import core.exceptions.WrongTypeException;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import org.junit.Test;

import java.math.BigDecimal;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.*;

public class NumberTest extends AbstractTest {

  @Test
  public void testIdentityElement() {
    assertEquals(0L, eval("(+)", env));
    assertEquals(1L, eval("(*)", env));
    assertEquals(1L, eval("(lcm)", env));
    assertEquals(0L, eval("(gcd)", env));
    assertEquals(TRUE, eval("(and)", env));
    assertEquals(FALSE, eval("(or)", env));
  }

  @Test
  public void testEvalNumbers() {
    assertEquals(1L, eval("1", env));
    assertEquals(-15L, eval("-15", env));
    assertEquals(-2.5d, eval("-2.5", env));
    assertEquals(5L, eval("#x5", env));
    assertEquals(5L, eval("#X5", env));
    assertEquals(15L, eval("#xf", env));
    assertEquals(15L, eval("#Xf", env));
    assertEquals(13L, eval("#b1101", env));
    assertEquals(13L, eval("#B1101", env));

    assertEquals(2.0, eval("#b1#", env));
    assertEquals(2.0, eval("#B1#", env));
    assertEquals(2L, eval("#B10", env));
    assertEquals(150.0, eval("15#", env));
    assertEquals(10.0, eval("+1#", env));
    assertEquals(100.0, eval("#i#d10#", env));
    assertEquals(100.0, eval("#d10#", env));
    assertEquals(4.0, eval("#b10#", env));
    assertEquals(150.0, eval("+15#", env));
    assertEquals(100L, eval("#e#d10#", env));
    assertEquals(150L, eval("#e15#", env));
    assertEquals(SCMBigRational.valueOf("101", "10"), eval("#e#d10.1", env));

    assertEquals(new SCMBigComplex(BigDecimal.ONE,  new BigDecimal(2)),  eval("1+2i", env));
    assertEquals(new SCMBigComplex(BigDecimal.ONE,  new BigDecimal(-2)), eval("1-2i", env));
    assertEquals(new SCMBigComplex(new BigDecimal(-1), new BigDecimal(2)),  eval("-1+2i", env));
    assertEquals(new SCMBigComplex(new BigDecimal(-1), new BigDecimal(-2)), eval("-1-2i", env));
    assertEquals(new SCMBigComplex(BigDecimal.ONE,  new BigDecimal(2)),  eval("#e1+2i", env));
    assertEquals(new SCMBigComplex(new BigDecimal("1.0"), new BigDecimal("2.0")), eval("#i1+2i", env));
    assertEquals(new SCMBigComplex(new BigDecimal("5"),   new BigDecimal("29")),  eval("#e#b101+11101i", env));
    assertEquals(new SCMBigComplex(new BigDecimal("5"),   new BigDecimal("29")),  eval("#e#b101+11101i", env));
    assertEquals(new SCMBigComplex(new BigDecimal("255.0"),  new BigDecimal("2987.9375")), eval("#x#iFf+BaB.fI", env));

    try {
      eval("+#", env);
      fail();
    } catch (UndefinedIdentifierException e) {
      // expected
    }
    try {
      eval("+1#1", env);
      fail();
    } catch (RuntimeException e) {
      // expected
    }
  }

  @Test
  public void testEvalMath() {
    assertEquals(null,  eval("(+ nil)", env));
    assertEquals(6L,  eval("(+ 1 2 3)", env));
    assertEquals(6L,  eval("(+ (byte 1) (int 2) (short 3))", env));
    assertEquals(2.1d, eval("(+ (byte 1) (double 1.1)", env));
    assertEquals(5.5, eval("(/ (+ 1 2 3 (- (* 2 2.5 2) 5)) 2)", env));
    assertEquals(5.0, eval("(/ 10.0 2)", env));
    assertEquals(SCMBigRational.valueOf("1", "10"), eval("(/ 10)", env));
    assertEquals(SCMBigRational.valueOf("13", "4"), eval("(/ 13 4)", env));
    assertEquals(2L, eval("(/ 10 5)", env));
    assertEquals(2d, eval("(/ 10.0 5)", env));
    assertEquals(2d, eval("(/ 10 5.0)", env));

    assertEquals(5L, eval("(abs 5)", env));
    assertEquals(5L, eval("(abs -5)", env));

    assertEquals(SCMBigRational.valueOf("9999", "3332"), eval("(/ 3332/9999)", env));
    assertEquals(SCMBigRational.valueOf("9999", "3332"), eval("(/ 1 3332/9999)", env));
    assertEquals(3.0009003601440574, eval("(/ 1.0 3332/9999)", env));
    try {
      eval("(+ nil nil)", env);
    } catch (NullPointerException e) {
      // expected
    }

    // abs
    try {
      eval("(abs)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("abs: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 0)", e.getMessage());
    }
    try {
      eval("(abs 1 2 3)", env);
      fail();
    } catch (ArityException e) {
      assertEquals("abs: arity mismatch; the expected number of arguments does not match the given number (expected: 1, given: 3)", e.getMessage());
    }
    try {
      eval("(abs \"not-a-number\")", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("abs: type mismatch; (expected: Real, given: \"not-a-number\")", e.getMessage());
    }

    // sqrt
    assertEquals(5d, eval("(sqrt 25)", env));
    assertEquals(3d, eval("(sqrt 9.0)", env));
    assertTrue(Double.isNaN((Double)eval("(sqrt -5)", env)));

    assertEquals(SCMBigRational.valueOf("1", "100"), eval("(/ 1 10 10)", env));
  }

  @Test
  public void testEvalNumericalComparison() {
    assertEquals(TRUE,  eval("(= 1 1 1)", env));
    assertEquals(FALSE, eval("(= 1 0 1)", env));
    assertEquals(TRUE,  eval("(= 0 0.0)", env));
    assertEquals(TRUE,  eval("(= 0.57 0.5700)", env));
    assertEquals(TRUE,  eval("(= 7 7.00)", env));
    assertEquals(TRUE,  eval("(= -234234/234 -234234/234 )", env));
    assertEquals(FALSE, eval("(= -134234/234 -234234/234 )", env));
    assertEquals(TRUE,  eval("(= 2000/3000 2/3)", env));
    assertEquals(TRUE,  eval("(= -9999999999999999999999999999999.0 -9999999999999999999999999999999.0)", env));

    assertEquals(TRUE,  eval("(> 2 1)", env));
    assertEquals(TRUE,  eval("(> 2 1.123)", env));
    assertEquals(TRUE,  eval("(>= 2 1.123)", env));
    assertEquals(TRUE,  eval("(>= 2.5 1.123)", env));
    assertEquals(TRUE,  eval("(<= -2.5 1.123)", env));
    assertEquals(TRUE,  eval("(< -2.5 1.123)", env));
  }

  @Test
  public void testNumberTheoreticDivision() {
    // quotient
    assertEquals(3L,  eval("(quotient 13 4)", env));
    assertEquals(3.0,  eval("(quotient 13.0 4)", env));
    assertEquals(1L,  eval("(quotient 5 5)", env));
    assertEquals(1.0,  eval("(quotient 5.0 5)", env));
    assertEquals(1.0,  eval("(quotient -5 -5.0)", env));
    assertEquals(-1L, eval("(quotient -5 5)", env));
    assertEquals(-1.0, eval("(quotient -5 5.)", env));
    try {
      eval("(quotient -10 0.0001)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("quotient: type mismatch; (expected: Integer, given: 1.0E-4)", e.getMessage());
    }
    try {
      eval("(quotient -10 0.0)", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("quotient: undefined for 0", e.getMessage());
    }

    // remainder
    assertEquals(-1L, eval("(remainder -13 4)", env));
    assertEquals(1L, eval("(remainder 13 -4)", env));
    assertEquals(-1L, eval("(remainder -13 -4)", env));
    assertEquals(-1.0, eval("(remainder -13 -4.0)", env));
    assertEquals(1L, eval("(remainder 13 4)", env));
    assertEquals(0L, eval("(remainder 10 2)", env));
    assertEquals(0d, eval("(remainder 10 2.0)", env));
    assertEquals(0d, eval("(remainder -10 2.0)", env));
    try {
      eval("(remainder -10 0.0001)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("remainder: type mismatch; (expected: Integer, given: 1.0E-4)", e.getMessage());
    }
    try {
      eval("(remainder -10 0.0)", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("remainder: undefined for 0", e.getMessage());
    }

    // modulo
    assertEquals(2L,  eval("(modulo 5 3)", env));
    assertEquals(2d,  eval("(modulo 5 3.0)", env));
    assertEquals(1L,  eval("(modulo 13 4)", env));
    assertEquals(-1L, eval("(modulo -13 -4)", env));
    try {
      eval("(modulo -10 0.0001)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("modulo: type mismatch; (expected: Integer, given: 1.0E-4)", e.getMessage());
    }
    try {
      eval("(modulo -10 0.0)", env);
      fail();
    } catch (ArithmeticException e) {
      assertEquals("modulo: undefined for 0", e.getMessage());
    }
    assertEquals(3L,  eval("(modulo -13 4)", env));
    assertEquals(-3L, eval("(modulo 13 -4)", env));
  }

  @Test
  public void testEvalIsZero() {
    assertEquals(TRUE,  eval("(zero? 0)", env));
    assertEquals(TRUE,  eval("(zero? 0.0)", env));
    assertEquals(TRUE,  eval("(zero? 0000000000000000000000000000.00000000000000000000000)", env));
    assertEquals(TRUE,  eval("(zero? 0/999999)", env));
    assertEquals(TRUE,  eval("(zero? -0/999999)", env));
    assertEquals(FALSE, eval("(zero? 1)", env));
    assertEquals(FALSE, eval("(zero? -5)", env));

    try {
      eval("(zero? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("zero?: type mismatch; (expected: Number, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalNegative() {
    assertEquals(FALSE, eval("(negative? 0)", env));
    assertEquals(FALSE, eval("(negative? 0.0)", env));
    assertEquals(FALSE, eval("(negative? 3/4)", env));
    assertEquals(TRUE,  eval("(negative? -3/4)", env));
    assertEquals(FALSE, eval("(negative? 9999999999999999999999999999999999.0)", env));
    assertEquals(TRUE,  eval("(negative? -9999999999999999999999999999999999.0)", env));
    assertEquals(FALSE, eval("(negative? 1)", env));
    assertEquals(FALSE, eval("(negative? (* -5 -6))", env));
    assertEquals(TRUE,  eval("(negative? -5)", env));
    try {
      eval("(negative? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("negative?: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalPositive() {
    assertEquals(FALSE, eval("(positive? 0)", env));
    assertEquals(FALSE, eval("(positive? 0.0)", env));
    assertEquals(TRUE,  eval("(positive? 3/4)", env));
    assertEquals(FALSE, eval("(positive? -3/4)", env));
    assertEquals(TRUE,  eval("(positive? 9999999999999999999999999999999999.0)", env));
    assertEquals(FALSE, eval("(positive? -9999999999999999999999999999999999.0)", env));
    assertEquals(TRUE,  eval("(positive? 1)", env));
    assertEquals(TRUE,  eval("(positive? (* -5 -6))", env));
    assertEquals(FALSE, eval("(positive? -5)", env));
    try {
      eval("(positive? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("positive?: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalEven() {
    assertEquals(TRUE,  eval("(even? 0)", env));
    assertEquals(TRUE,  eval("(even? 0.0)", env));
    assertEquals(TRUE,  eval("(even? 4)", env));
    assertEquals(TRUE,  eval("(even? 100)", env));
    assertEquals(FALSE, eval("(even? 1)", env));
    assertEquals(TRUE,  eval("(even? (* -5 -6))", env));
    assertEquals(FALSE, eval("(even? -5)", env));
    try {
      eval("(even? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("even?: type mismatch; (expected: Integer, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalOdd() {
    assertEquals(FALSE, eval("(odd? 0)", env));
    assertEquals(FALSE, eval("(odd? 0.0)", env));
    assertEquals(FALSE, eval("(odd? 4)", env));
    assertEquals(FALSE, eval("(odd? 100)", env));
    assertEquals(TRUE,  eval("(odd? 1)", env));
    assertEquals(FALSE, eval("(odd? 4)", env));
    assertEquals(FALSE, eval("(odd? (* -5 -6))", env));
    assertEquals(TRUE,  eval("(odd? -5)", env));
    try {
      eval("(odd? \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("odd?: type mismatch; (expected: Integer, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalRound() {
    assertEquals(0L,   eval("(round 0)",    env));
    assertEquals(4L,   eval("(round 4)",    env));
    assertEquals(-4L,  eval("(round -4)",   env));
    assertEquals(0.0,  eval("(round 0.0)",  env));
    assertEquals(1.0,  eval("(round 1.0)",  env));
    assertEquals(2.0,  eval("(round 1.5)",  env));
    assertEquals(-2.0, eval("(round -1.5)", env));
    assertEquals(2.0,  eval("(round 2.5)",  env));
    assertEquals(-0.0, eval("(round -0.5)", env));
    assertEquals(-2.0, eval("(round -1.7)", env));
    assertEquals(4.0,  eval("(round 3.7)",  env));
    assertEquals(3.0,  eval("(round 2.7)",  env));
    assertEquals(2.0,  eval("(round 2.5)",  env));
    try {
      eval("(round \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("round: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalFloor() {
    assertEquals(0L,   eval("(floor 0)",    env));
    assertEquals(4L,   eval("(floor 4)",    env));
    assertEquals(-5.0, eval("(floor -4.3)", env));
    assertEquals(3.0,  eval("(floor 3.5)",  env));
    assertEquals(1.0,  eval("(floor 1.2)",  env));
    assertEquals(-2.0, eval("(floor -1.2)", env));
    try {
      eval("(floor \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("floor: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalCeiling() {
    assertEquals(0L,   eval("(ceiling 0)",    env));
    assertEquals(4L,   eval("(ceiling 4)",    env));
    assertEquals(-4.0, eval("(ceiling -4.3)", env));
    assertEquals(4.0,  eval("(ceiling 3.5)",  env));
    assertEquals(2.0,  eval("(ceiling 1.2)",  env));
    assertEquals(-1.0, eval("(ceiling -1.2)", env));
    try {
      eval("(ceiling \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("ceiling: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalTruncate() {
    assertEquals(0L,   eval("(truncate 0)",    env));
    assertEquals(4L,   eval("(truncate 4)",    env));
    assertEquals(-4L,  eval("(truncate -4)",   env));
    assertEquals(3.0,  eval("(truncate 3.5)",  env));
    assertEquals(-3.0, eval("(truncate -3.5)", env));
    assertEquals(2.0,  eval("(truncate 2.2)",  env));
    assertEquals(-1.0, eval("(truncate -1.2)", env));
    try {
      eval("(truncate \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("truncate: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalMax() {
    assertEquals(0L,   eval("(max 0)",    env));
    assertEquals(5.0,  eval("(max 5.0)",  env));
    assertEquals(-5.0, eval("(max -5.0)", env));
    assertEquals(-5.0, eval("(max -6 -7 -5.0)", env));
    assertEquals(7L,  eval("(max 6 7 5.0)",    env));

    try {
      eval("(max \"test\" 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("max: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }

    try {
      eval("(max 0 \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("max: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalMin() {
    assertEquals(0L,   eval("(min 0)",    env));
    assertEquals(5.0,  eval("(min 5.0)",  env));
    assertEquals(-5.0, eval("(min -5.0)", env));
    assertEquals(-7L, eval("(min -6 -7 -5.0)", env));
    assertEquals(5.0,  eval("(min 6 7 5.0)",    env));
    try {
      eval("(min \"test\" 1 2 3)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("min: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }

    try {
      eval("(min 0 \"test\")", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("min: type mismatch; (expected: Real, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testEvalGCD() {
    // gcd of no args is 0
    assertEquals(0L, eval("(gcd)", env));
    // gcd of 0(s) is 0
    assertEquals(0L, eval("(gcd 0)", env));
    assertEquals(0d, eval("(gcd 0.0)", env));
    assertEquals(0L, eval("(gcd 0 0)", env));
    assertEquals(0d, eval("(gcd 0 0.0)", env));
    assertEquals(5L, eval("(gcd 5 0)", env));
    assertEquals(5d, eval("(gcd 5.0 0)", env));
    assertEquals(5L, eval("(gcd 0 5)", env));

    // gcd of n is n
    assertEquals(5L, eval("(gcd 5)", env));
    assertEquals(5L, eval("(gcd -5)", env));

    // gcd of multiple numbers
    assertEquals(5L, eval("(gcd 5 10)", env));
    assertEquals(1L, eval("(gcd 3 6 8)", env));

    assertEquals(3d, eval("(gcd 3.0 6)", env));
    assertEquals(40000d, eval("(gcd 200000.0 40000.0)", env));
    assertEquals(new BigDecimal("8.8817841970012523233890533447265625E-16"), eval("(gcd 3.3 6)", env));
    assertEquals(9L, eval("(gcd 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 9)", env));

    /* Check switch from Double to BigDecimal for big numbers */
    assertEquals(3L,   eval("(gcd 99999999999999999 123)", env));
    assertEquals(3L,   eval("(gcd 999999999999999999 123)", env));
    assertEquals(3L,   eval("(gcd 9999999999999999999 123)", env));
    assertEquals(123L, eval("(gcd 99999999999999999999 123)", env));
    assertEquals(3L,   eval("(gcd 999999999999999999999 123)", env));
    assertEquals(3L,   eval("(gcd 9999999999999999999999 123)", env));
    assertEquals(1L,   eval("(gcd (expt 17 34) (expt 21 19))", env));
  }

  @Test
  public void testEvalLCM() {
    // lcm of no args is 0
    assertEquals(1L, eval("(lcm)", env));
    // lcm of 0(s) is 0
    assertEquals(0L, eval("(lcm 0)", env));
    assertEquals(0d, eval("(lcm 0.0)", env));
    assertEquals(0L, eval("(lcm 0 0)", env));

    // lcm of n is n
    assertEquals(5L, eval("(lcm 5)", env));
    assertEquals(5d, eval("(lcm 5.0)", env));
    assertEquals(5L, eval("(lcm -5)", env));

    // lcm of multiple numbers
    assertEquals(10L, eval("(lcm 5 10)", env));
    assertEquals(24L, eval("(lcm 3 6 8)", env));
    assertEquals(24d, eval("(lcm 3 6 8.0)", env));

    assertEquals(6d, eval("(lcm 3.0 6)", env));
    assertEquals(200000d, eval("(lcm 200000.0 40000.0)", env));
    assertEquals(new Double("2.2292818155483952E+16"), eval("(lcm 3.3 6)", env));

    String big = "99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999";
    assertEquals(new BigDecimal(big), eval(String.format("(lcm %s 9)", big), env));

    assertEquals(new BigDecimal("9066296685449845496062520942090242184133343279738102243137866423949"),
                 eval("(lcm (expt 17 34) (expt 21 19))", env));
    assertEquals(12L, eval("(lcm 1/2 4/5 3/56)", env));
  }

  @Test
  public void testEvalExpt() {
    assertEquals(1L, eval("(expt 9 0)", env));
    assertEquals(0L, eval("(expt 0 10)", env));
    assertEquals(1L, eval("(expt 1 1)", env));
    assertEquals(8L, eval("(expt 2 3)", env));
    assertEquals(16777216L, eval("(expt 4 12)", env));
    assertEquals(25L, eval("(expt -5 2)", env));
    assertEquals(-125L, eval("(expt -5 3)", env));
    assertEquals(13.489468760533386, eval("(expt 2.2 3.3)", env));
    try {
      eval("(expt \"test\" 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("expt: type mismatch; (expected: Number, given: \"test\")", e.getMessage());
    }
    try {
      eval("(expt 1)", env);
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("expt: arity mismatch; the expected number of arguments does not match the given number (expected: 2, given: 1)", e.getMessage());
    }
    assertEquals(1L, eval("(expt 0 0.0)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt 0.0 -5)", env));
    assertEquals(0d, eval("(expt 0.0 5)", env));
    assertEquals(Double.NEGATIVE_INFINITY, eval("(expt -0.0 -5)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt -0.0 -6)", env));
    assertEquals(-0d, eval("(expt -0.0 5)", env));
    assertEquals(0d, eval("(expt -0.0 6)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt +inf.0 2)", env));
    assertEquals(0d, eval("(expt +inf.0 -2)", env));
    assertEquals(0d, eval("(expt -inf.0 -2)", env));
    assertEquals(-0d, eval("(expt -inf.0 -3)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt -inf.0 2)", env));
    assertEquals(Double.NEGATIVE_INFINITY, eval("(expt -inf.0 3)", env));
    assertEquals(0L, eval("(expt 0 +inf.0)", env));
    try {
      eval("(expt 0 -inf.0)", env);
      fail();
    } catch (ArithmeticException e) {
      // success
    }
    assertEquals(0d, eval("(expt 0.5 +inf.0)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt 5 +inf.0)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt 0.5 -inf.0)", env));
    assertEquals(0d, eval("(expt 5 -inf.0)", env));
    assertEquals(Double.NaN, eval("(expt 2+3i -inf.0)", env));
    assertEquals(Double.NaN, eval("(expt 0 +nan.0)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt 2 2147483648", env));
    assertEquals(Double.NEGATIVE_INFINITY, eval("(expt -2 2147483647", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(expt -2 2147483648", env));
    assertEquals(9.610000000000001, eval("(expt -3.1 2)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("-0.0029666184064708"), new BigDecimal("0.0009639127514694")),
                 eval("(expt -3.1 -5.1)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("-0.0007509092057438"), new BigDecimal("-0.0001938306001686")),
                 eval("(expt -3.1 2+3i)", env));
    assertEquals(new SCMBigComplex(new BigDecimal("-2.4728383E-9"), new BigDecimal("9.0752993E-9")),
                 eval("(expt -2.3-3.4i -4.5-5.6i)", env));
  }

  @Test
  public void testBigDecimal() {

    String big0 = "2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
    assertEquals(new BigDecimal(big0), eval(big0, env));

    BigDecimal big1 = new BigDecimal("46253220748483971668312221557921994865355615937726643119142534763573384386717010179" +
        "33035570379376866259256597659610145236756255853523897837700465755231009606767424706" +
        "73348011864143704094117912896787287887428473700404535710995567015427340013704721113" +
        "33316343260021388334157118647272603383652782933091786683436259682248241271823609303" +
        "74088996645418723950501545025386234033486857262740020740808886229945286599837304752" +
        "25837944037056491016035642078654527374207462210630264065442615967117912099739418879" +
        "84132513");

    assertEquals(big1, eval("(expt 33 333)", env));
    assertEquals(TRUE, eval(String.format("(number? %s)", big0), env));
    assertEquals(TRUE, eval(String.format("(complex? %s)", big0), env));

    assertEquals(new BigDecimal(big0), eval(String.format("(* (/ %s 10) 10)", big0), env));
    assertEquals(new BigDecimal(big0).multiply(new BigDecimal("2")), eval(String.format("(+ %s %s)", big0, big0), env));
    assertEquals(new BigDecimal(big0).multiply(new BigDecimal("2")).subtract(new BigDecimal(big0)),
        eval(String.format("(- (* 2 %s) %s)", big0, big0), env));

    assertEquals(new BigDecimal("999999999999997000000000000002999999999999999"),
                 eval("(* 999999999999999 999999999999999 999999999999999)", env));

    assertEquals(new BigDecimal(big0), eval(String.format("(truncate (+ 0.2 %s))", big0),  env));
    assertEquals(new BigDecimal(big0).negate(), eval(String.format("(truncate (+ 0.2 -%s))", big0),  env));
    assertEquals(new BigDecimal(big0), eval(String.format("(floor (+ 0.2 %s))", big0),  env));
    assertEquals(new BigDecimal(big0).add(BigDecimal.ONE),
        eval(String.format("(ceiling (+ 0.2 %s))", big0),  env));

    assertEquals(new BigDecimal(big0), eval(String.format("(abs -%s)", big0),  env));
    assertEquals(new BigDecimal(big0).add(BigDecimal.ONE),
        eval(String.format("(max (+ 1 %s) %s)", big0, big0),  env));

    assertEquals(new BigDecimal(big0),
        eval(String.format("(min (+ 1 %s) %s)", big0, big0),  env));

    String big2 = "941737268473075634481294063531333847658485002458168527101639838005582185517473483816983389228732066437165294377295109210176795859047876399460771530181828861843994801526320659067260600443063376955200810073997787724454002350759571876705644517946943898492214066331998886559185229835330687165577365519449395424366904222913306696961330084086377946063169138303897697242206192836209273444873251023411764271944704088313845446589768727760791185170266144604537045173629663045739300767985189493967771010336173962367396474652866334212802605674879313278209206179544726008444885447395757991883875945457869103573901612777316112247438629624081718143710269108788904389008167209091151002216893051746019091645742839251513268837094248809018521046734530253606053753445604156050903737280600427015788467630468023527367174845920094011539693975275654700093627716413";
    assertEquals(1L, eval(String.format("(modulo %s 4)", big2), env));
    assertEquals(-2L, eval(String.format("(modulo %s -5)", big2), env));

    assertEquals(1L, eval(String.format("(remainder %s 4)", big2), env));
    assertEquals(3L, eval(String.format("(remainder %s -5)", big2), env));

    String quotientResult1 = "470868634236537817240647031765666923829242501229084263550819919002791092758736741908491694614366033218582647188647554605088397929523938199730385765090914430921997400763160329533630300221531688477600405036998893862227001175379785938352822258973471949246107033165999443279592614917665343582788682759724697712183452111456653348480665042043188973031584569151948848621103096418104636722436625511705882135972352044156922723294884363880395592585133072302268522586814831522869650383992594746983885505168086981183698237326433167106401302837439656639104603089772363004222442723697878995941937972728934551786950806388658056123719314812040859071855134554394452194504083604545575501108446525873009545822871419625756634418547124404509260523367265126803026876722802078025451868640300213507894233815234011763683587422960047005769846987637827350046813858206";
    assertEquals(new BigDecimal(quotientResult1), eval(String.format("(quotient %s 2)", big2), env));
    assertEquals(2L, eval(String.format("(quotient %s (quotient %s 2))", big2, big2), env));

    assertEquals(TRUE, eval(String.format("(eqv? %s %s)", big2, big2), env));
    assertEquals(TRUE, eval(String.format("(<= %s %s)", big2, big2), env));
    assertEquals(FALSE, eval(String.format("(< %s %s)", big2, big2), env));
    assertEquals(TRUE, eval(String.format("(> (+ 1 %s) %s)", big2, big2), env));
    assertEquals(TRUE, eval(String.format("(< (+ 1 2) %s)", big2), env));

    assertEquals(Double.POSITIVE_INFINITY, eval(String.format("(sqrt %s)", big2), env));
    assertEquals(new BigDecimal("-99999999999999999999999999999999999999999999999999"),
                 eval("(- 99999999999999999999999999999999999999999999999999)", env));
  }

  @Test
  public void testEvalIsInteger() {
    assertEquals(TRUE,  eval("(integer? 0)", env));
    assertEquals(TRUE,  eval("(integer? 0.0)", env));
    assertEquals(TRUE,  eval("(integer? 4)", env));
    assertEquals(TRUE,  eval("(integer? 100)", env));
    assertEquals(TRUE,  eval("(integer? 1)", env));
    assertEquals(TRUE,  eval("(integer? (* -5 -6))", env));
    assertEquals(TRUE,  eval("(integer? -5)", env));
    assertEquals(FALSE, eval("(integer? -5.4)", env));
    assertEquals(FALSE, eval("(integer? 3.14)", env));
    assertEquals(FALSE, eval("(integer? .123)", env));
    assertEquals(FALSE, eval("(integer? \"test\")", env));
  }

  @Test
  public void testNumberToString() {
    assertEquals("5",  eval("(number->string 5)", env));
    assertEquals("5.5",  eval("(number->string 5.5)", env));
    assertEquals("9999999999999999999999999999999",  eval("(number->string #d9999999999999999999999999999999)", env));
    assertEquals("9999999999999999999999999999999.5",  eval("(number->string 9999999999999999999999999999999.5)", env));

    assertEquals("5", eval("(number->string #b101)", env));
    assertEquals("309461373397964671249896789", eval("(number->string #b1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101)", env));
    assertEquals("309461373397964671249896789", eval("(number->string #B1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101)", env));

    assertEquals("449", eval("(number->string #o701)", env));
    assertEquals("29889", eval("(number->string #o72301)", env));
    assertEquals("1237940039285380274899121345", eval("(number->string #o777777777777777777777777772301)", env));
    assertEquals("1237940039285380274899121345", eval("(number->string #O777777777777777777777777772301)", env));

    assertEquals("0", eval("(number->string #x0)", env));
    assertEquals("15", eval("(number->string #xf)", env));
    assertEquals("255", eval("(number->string #xff)", env));
    assertEquals("324518553658426726783156020576255", eval("(number->string #xfffffffffffffffffffffffffff)", env));

    assertEquals("777777777777777777777777777777777777", eval("(number->string #xfffffffffffffffffffffffffff 8)", env));
    assertEquals("11111111", eval("(number->string #xff 2)", env));
    assertEquals("1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101", eval("(number->string #b1111111111111010111111101010101010111011010101101010101010101110110101010101010101010101 2)", env));
    assertEquals("3a885", eval("(number->string 239749 16)", env));

    assertEquals("-3a885", eval("(number->string -239749 16)", env));
    assertEquals("-777777777777777777777777777777777777", eval("(number->string #x-fffffffffffffffffffffffffff 8)", env));
  }

  @Test
  public void testStringToNumber() {
    assertEquals(100L, eval("(string->number \"100\")", env));
    assertEquals(256L, eval("(string->number \"100\" 16)", env));
    assertEquals(FALSE, eval("(string->number \"hello\")", env));
    assertEquals(57L, eval("(string->number \"111\" 7)", env));
    assertEquals(7L, eval("(string->number \"#b111\" 7)", env));
    assertEquals(new BigDecimal("26623333280885243903"), eval("(string->number \"bbbbbbbbbbbbbbbbbb\" 12)", env));
    assertEquals(new BigDecimal("26623333280885243903"), eval("(string->number \"BBBBBBBBBBBBBBBBBB\" 12)", env));
    assertEquals(new BigDecimal("110573323209400121422731899656355381011962890624"), eval("(string->number \"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\" 15)", env));
    assertEquals(FALSE, eval("(string->number \"eeef\" 15)", env));
    assertEquals(85L, eval("(string->number \"1010101\" 2)", env));
    assertEquals(new BigDecimal("289264344747772786367397236066475587972918828808734345141483382767615"), eval("(string->number \"#xababaabababababababababababababafffffffffffffffffffffffff\")", env));
    assertEquals(1500.0, eval("(string->number \"15##\")", env));
    assertEquals(FALSE, eval("(string->number \"1234#d\")", env));
    assertEquals(100.0, eval("(string->number \"1e2\")", env));
    assertEquals(new BigDecimal("0.5"), eval("(string->number \"#b1e-1\")", env));
    assertEquals(new BigDecimal("6161212520618990239744.0"), eval("(string->number \"#o1234e+25\")", env));
    assertEquals(FALSE, eval("(string->number \"#\")", env));
    assertEquals(FALSE, eval("(string->number \"#e\")", env));
    assertEquals(FALSE, eval("(string->number \"##\")", env));
    assertEquals(FALSE, eval("(string->number \"#e#\")", env));
    assertEquals(FALSE, eval("(string->number \"##e\")", env));
    assertEquals(FALSE, eval("(string->number \"###\")", env));
    assertEquals(FALSE, eval("(string->number \"###e\")", env));
    assertEquals(FALSE, eval("(string->number \"#e##\")", env));
    assertEquals(FALSE, eval("(string->number \"#e#e\")", env));
    assertEquals(FALSE, eval("(string->number \"eeef\")", env));
    assertEquals(FALSE, eval("(string->number \"#e#i1\")", env));
    assertEquals(FALSE, eval("(string->number \"#e#b#\")", env));
    assertEquals(FALSE, eval("(string->number \"#e#i#e\")", env));
    assertEquals(FALSE, eval("(string->number \"1.1.1\")", env));
    try {
      eval("(string->number \"#b1e5\")", env);
      fail();
    } catch (IllegalSyntaxException e) {
      assertEquals("read: bad exponent: 1e5", e.getMessage());
    }
  }

  @Test
  public void testSpecialNumbers() {
    assertEquals(Double.NaN, eval("+nan.0", env));
    assertEquals(Double.NaN, eval("-nan.0", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("+inf.0", env));
    assertEquals(Double.NEGATIVE_INFINITY, eval("-inf.0", env));
  }

  @Test
  public void testThresholds() {
    String chars = "123456789abcdef";
    for (int r = 2; r <= 16; r++) {
      StringBuilder sb = new StringBuilder();
      char ch = chars.charAt(r - 2);
      for (int c = 1; c <= 70; c++) {
        sb.append(ch);
        assertNotEquals(FALSE, eval(String.format("(string->number \"%s\" %s)", sb.toString(), r), env));
      }
    }
  }

  @Test
  public void testQuotientViaTruncate() {
    // quotient = (truncate (/ n m))
    eval("(define // (lambda (n m) (truncate (/ n m))))", env);
    assertEquals(TRUE, eval("(= (quotient 5 4) (// 5 4))", env));
    assertEquals(TRUE, eval("(= (quotient 5 4.0) (// 5 4.0))", env));
    assertEquals(TRUE, eval("(= (quotient -5 4.0) (// -5 4.0))", env));
    assertEquals(TRUE, eval("(= (quotient -5999999999999999999999999999999999999 4.0) (// -5999999999999999999999999999999999999 4.0))", env));
  }

  @Test
  public void testIsRational() {
    assertEquals(TRUE, eval("(rational? 0)", env));
    assertEquals(TRUE, eval("(rational? 1.0)", env));
    assertEquals(TRUE, eval("(rational? -234234/234234)", env));
    assertEquals(TRUE, eval("(rational? -83457348957348573498573489573489573489583457389457349534.3489534895)", env));

    assertEquals(FALSE, eval("(rational? +inf.0)", env));
    assertEquals(FALSE, eval("(rational? -inf.0)", env));
    assertEquals(FALSE, eval("(rational? +nan.0)", env));
    assertEquals(FALSE, eval("(rational? -nan.0)", env));
    assertEquals(FALSE, eval("(rational? \"test\")", env));
    assertEquals(FALSE, eval("(rational? 12-4i)", env));
  }

  @Test
  public void testIsReal() {
    assertEquals(TRUE, eval("(real? 0)", env));
    assertEquals(TRUE, eval("(real? 1.0)", env));
    assertEquals(TRUE, eval("(real? -234234/234234)", env));
    assertEquals(TRUE, eval("(real? -83457348957348573498573489573489573489583457389457349534.3489534895)", env));
    assertEquals(TRUE, eval("(real? +inf.0)", env));
    assertEquals(TRUE, eval("(real? -inf.0)", env));
    assertEquals(TRUE, eval("(real? +nan.0)", env));
    assertEquals(TRUE, eval("(real? -nan.0)", env));
    assertEquals(FALSE, eval("(real? \"test\")", env));
  }

  @Test
  public void testIsNumber() {
    assertEquals(TRUE, eval("(number? 0)", env));
    assertEquals(TRUE, eval("(number? 1.0)", env));
    assertEquals(TRUE, eval("(number? -234234/234234)", env));
    assertEquals(TRUE, eval("(number? -83457348957348573498573489573489573489583457389457349534.3489534895)", env));
    assertEquals(TRUE, eval("(number? +inf.0)", env));
    assertEquals(TRUE, eval("(number? -inf.0)", env));
    assertEquals(TRUE, eval("(number? +nan.0)", env));
    assertEquals(TRUE, eval("(number? -nan.0)", env));
    assertEquals(TRUE, eval("(number? -1/3)", env));
    assertEquals(FALSE, eval("(number? \"test\")", env));
  }

  @Test
  public void testIsComplex() {
    assertEquals(TRUE, eval("(complex? 0)", env));
    assertEquals(TRUE, eval("(complex? 1.0)", env));
    assertEquals(TRUE, eval("(complex? -234234/234234)", env));
    assertEquals(TRUE, eval("(complex? -83457348957348573498573489573489573489583457389457349534.3489534895)", env));
    assertEquals(TRUE, eval("(complex? +inf.0)", env));
    assertEquals(TRUE, eval("(complex? -inf.0)", env));
    assertEquals(TRUE, eval("(complex? +nan.0)", env));
    assertEquals(TRUE, eval("(complex? -nan.0)", env));
    assertEquals(TRUE, eval("(complex? -1/3)", env));
    assertEquals(FALSE, eval("(complex? \"test\")", env));
  }

  @Test
  public void testIsExact() {
    assertEquals(TRUE, eval("(exact? 0)", env));
    assertEquals(TRUE, eval("(exact? -3/5)", env));
    assertEquals(TRUE, eval("(exact? 99999999999999999999999999999999999999999999999999999999999999999999)", env));

    assertEquals(FALSE, eval("(exact? -2.5)", env));
    assertEquals(FALSE, eval("(exact? +inf.0)", env));
    assertEquals(FALSE, eval("(exact? -inf.0)", env));
    assertEquals(FALSE, eval("(exact? +nan.0)", env));
    assertEquals(FALSE, eval("(exact? -nan.0)", env));

    try {
      eval("(exact? \"test\")", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("exact?: type mismatch; (expected: Number, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testIsInexact() {
    assertEquals(FALSE, eval("(inexact? 0)", env));
    assertEquals(FALSE, eval("(inexact? -3/5)", env));
    assertEquals(FALSE, eval("(inexact? 99999999999999999999999999999999999999999999999999999999999999999999)", env));

    String[] trues = {"(inexact? -2.5)", "(inexact? +inf.0)", "(inexact? -inf.0)", "(inexact? +nan.0)",
                      "(inexact? -nan.0)", };
    assertAllEqual(TRUE, trues, env);
    try {
      eval("(inexact? \"test\")", env);
      fail();
    } catch (WrongTypeException e) {
      assertEquals("inexact?: type mismatch; (expected: Number, given: \"test\")", e.getMessage());
    }
  }

  @Test
  public void testNumerator() {
    assertEquals(1L, eval("(numerator 1)", env));
    assertEquals(-1234L, eval("(numerator -1234)", env));
    assertEquals(-1234.0, eval("(numerator -1234.0)", env));
    assertEquals(17L, eval("(numerator 17/4)", env));
    assertEquals(2589569785738035.0, eval("(numerator 2.3)", env));
  }

  @Test
  public void testDenominator() {
    assertEquals(1L, eval("(denominator 1)", env));
    assertEquals(1L, eval("(denominator -1234)", env));
    assertEquals(1.0, eval("(denominator -1234.0)", env));
    assertEquals(4L, eval("(denominator 17/4)", env));
    assertEquals(1125899906842624.0, eval("(denominator 2.3)", env));
    assertEquals(SCMBigRational.valueOf("9347593487539475934753495739845734957349857349573495873459374589347593475394857393453454353", "10000000000"), eval("(inexact->exact 934759348753947593475349573984573495734985734957349587345937458934759347539485739.3453454353)", env));
  }

  @Test
  public void testExp() {
    assertEquals(1L, eval("(exp 0)", env));
    assertEquals(2.718281828459045, eval("(exp 1)", env));
    assertEquals(59874.14171519782, eval("(exp 11)", env));
    assertEquals(0.36787944117144233, eval("(exp -1)", env));
    assertEquals(2.117000016612675, eval("(exp 3/4)", env));
    assertEquals(2.718281828459045, eval("(exp 1/1)", env));
    assertEquals(Double.POSITIVE_INFINITY, eval("(exp 999999999)", env));
    assertEquals(0d, eval("(exp -999999999)", env));
  }

  @Test
  public void testLog() {
    assertEquals(0L, eval("(log 1)", env));
    assertEquals(2.3978952727983707, eval("(log 11)", env));
    assertEquals(Double.NaN, eval("(log -1)", env));
    assertEquals(-0.2876820724517809, eval("(log 3/4)", env));
    assertEquals(0L, eval("(log 1/1)", env));
    assertEquals(20.72326583594641, eval("(log 999999999)", env));
    assertEquals(Double.NaN, eval("(log -999999999)", env));
    assertEquals(Double.NEGATIVE_INFINITY, eval("(log 0.0)", env));
    assertEquals(110.52408446371419, eval("(log 999999999999999999999999999999999999999999999999)", env));
    assertEquals(135.8525204866487, eval("(log 99999999999999999999999999999999999999999999999999999999999)", env));
    // FIXME assertEquals(135630.27870981014, eval("(log (expt 3 123456))", env));
//    assertEquals(135630.27870981017, eval("(log (expt 3 123456))", env));
    try {
      assertEquals(1L, eval("(log 0)", env));
      fail();
    } catch (ArithmeticException e) {
      assertEquals("log: undefined for 0", e.getMessage());
    }
    try {
      assertEquals(1L, eval("(log 0/1)", env));
      fail();
    } catch (ArithmeticException e) {
      assertEquals("log: undefined for 0", e.getMessage());
    }
  }

  @Test
  public void testFindBetween() {
    String findBetween = "(define (find-between lo hi)" +
                         "  (if (integer? lo)" +
                         "      lo" +
                         "    (let ((lo-int (floor lo))" +
                         "          (hi-int (floor hi)))" +
                         "      (if (< lo-int hi-int)" +
                         "          (+ 1 lo-int)" +
                         "        (+ lo-int" +
                         "           (/ (find-between (/ (- hi lo-int)) (/ (- lo lo-int)))))))))";
    eval(findBetween, env);
    assertEquals(SCMBigRational.valueOf("1", "3"), eval("(find-between 3332/9999 3334/9999)", env));
  }

  @Test
  public void testRationalize() {
    assertEquals(0L, eval("(rationalize 1/3 1/3)", env));
    assertEquals(SCMBigRational.valueOf("1", "3"), eval("(rationalize 1/3 1/9999)", env));
    assertEquals(0L, eval("(rationalize 2/3 1)", env));
    assertEquals(2333L, eval("(rationalize 2335 2)", env));
    assertEquals(-2L, eval("(rationalize -5 3)", env));
  }
}
