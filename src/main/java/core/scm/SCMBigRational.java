package core.scm;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * TODO Create SCMRational class for small rational numbers
 */
public class SCMBigRational extends Number implements ISCMClass, Comparable<SCMBigRational> {

  public static final SCMBigRational ZERO = new SCMBigRational(BigInteger.ZERO, BigInteger.ONE);

  private BigInteger numerator;
  private BigInteger denominator;

  public SCMBigRational(BigInteger numerator, BigInteger denominator) {
    init(numerator, denominator);
  }

  private void init(BigInteger numerator, BigInteger denominator) {
    if (denominator.equals(BigInteger.ZERO)) {
      throw new ArithmeticException("/ by zero");
    }
    // reduce fraction
    BigInteger g = numerator.gcd(denominator);
    this.numerator = numerator.divide(g);
    this.denominator = denominator.divide(g);

    // to ensure invariant that denominator is positive
    if (this.denominator.compareTo(BigInteger.ZERO) < 0) {
      this.denominator = this.denominator.negate();
      this.numerator = this.numerator.negate();
    }
  }

  public BigInteger getNumerator() {
    return numerator;
  }

  public BigInteger getDenominator() {
    return denominator;
  }

  public BigDecimal toBigDecimal() {
    return new BigDecimal(numerator).divide(new BigDecimal(denominator), BigDecimal.ROUND_HALF_EVEN);
  }

  public boolean isDenominatorEqualToOne() {
    return denominator.compareTo(BigInteger.ONE) == 0;
  }

  public boolean isZero() {
    return compareTo(ZERO) == 0;
  }

  public boolean isPositive() {
    return compareTo(ZERO) > 0;
  }

  public boolean isNegative() {
    return compareTo(ZERO) < 0;
  }

  public SCMBigRational abs() {
    return new SCMBigRational(numerator.abs(), denominator.abs());
  }

  public SCMBigRational ceiling() {
    if (isPositive()) {
      return new SCMBigRational(
        new BigDecimal(numerator).divide(new BigDecimal(denominator), BigDecimal.ROUND_UP).toBigInteger(),
        BigInteger.ONE);
    } else {
      return new SCMBigRational(
        new BigDecimal(numerator).divide(new BigDecimal(denominator), BigDecimal.ROUND_DOWN).toBigInteger(),
        BigInteger.ONE);
    }
  }

  public SCMBigRational floor() {
    if (isPositive()) {
      return new SCMBigRational(
        new BigDecimal(numerator).divide(new BigDecimal(denominator), BigDecimal.ROUND_DOWN).toBigInteger(),
        BigInteger.ONE);
    } else {
      return new SCMBigRational(
        new BigDecimal(numerator).divide(new BigDecimal(denominator), BigDecimal.ROUND_UP).toBigInteger(),
        BigInteger.ONE);
    }
  }

  public SCMBigRational round() {
    BigDecimal round = new BigDecimal(numerator).divide(new BigDecimal(denominator), BigDecimal.ROUND_HALF_EVEN)
                                                .round(MathContext.DECIMAL32);
    return new SCMBigRational(round.toBigInteger(), BigInteger.ONE);
  }

  public SCMBigRational truncate() {
    if (isNegative()) {
      return ceiling();
    } else {
      return floor();
    }
  }

  @Override
  public String toString() {
    if (denominator.equals(BigInteger.ONE)) {
      return numerator.toString();
    } else {
      return numerator + "/" + denominator;
    }
  }

  @Override
  public int compareTo(SCMBigRational other) {
    return this.numerator.multiply(other.denominator).compareTo(this.denominator.multiply(other.numerator));
  }
  @Override
  public boolean equals(Object y) {
    if (y == this) return true;
    if (y == null) return false;
    if (y.getClass() != this.getClass()) return false;
    SCMBigRational b = (SCMBigRational) y;
    return compareTo(b) == 0;
  }

  @Override
  public int hashCode() {
    return this.toString().hashCode();
  }

  public SCMBigRational multiply(SCMBigRational other) {
    return new SCMBigRational(this.numerator.multiply(other.numerator), this.denominator.multiply(other.denominator));
  }

  public SCMBigRational plus(SCMBigRational other) {
    BigInteger numerator   = this.numerator.multiply(other.denominator).add(other.numerator.multiply(this.denominator));
    BigInteger denominator = this.denominator.multiply(other.denominator);
    return new SCMBigRational(numerator, denominator);
  }

  public SCMBigRational negate() {
    return new SCMBigRational(numerator.negate(), denominator);
  }

  public SCMBigRational minus(SCMBigRational other) {
    return this.plus(other.negate());
  }

  public SCMBigRational reciprocal() {
    return new SCMBigRational(denominator, numerator);
  }

  public SCMBigRational divide(SCMBigRational other) {
    return this.multiply(other.reciprocal());
  }

  @Override
  public int intValue() {
    int SCALE = 32;
    BigDecimal numerator   = new BigDecimal(this.numerator);
    BigDecimal denominator = new BigDecimal(this.denominator);
    BigDecimal quotient    = numerator.divide(denominator, SCALE, RoundingMode.HALF_EVEN);
    return quotient.intValue();
  }

  @Override
  public long longValue() {
    int SCALE = 32;
    BigDecimal numerator   = new BigDecimal(this.numerator);
    BigDecimal denominator = new BigDecimal(this.denominator);
    BigDecimal quotient    = numerator.divide(denominator, SCALE, RoundingMode.HALF_EVEN);
    return quotient.longValue();
  }

  @Override
  public float floatValue() {
    int SCALE = 32;
    BigDecimal numerator   = new BigDecimal(this.numerator);
    BigDecimal denominator = new BigDecimal(this.denominator);
    BigDecimal quotient    = numerator.divide(denominator, SCALE, RoundingMode.HALF_EVEN);
    return quotient.floatValue();
  }

  @Override
  public double doubleValue() {
    int SCALE = 32;
    BigDecimal numerator   = new BigDecimal(this.numerator);
    BigDecimal denominator = new BigDecimal(this.denominator);
    BigDecimal quotient    = numerator.divide(denominator, SCALE, RoundingMode.HALF_EVEN);
    return quotient.doubleValue();
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.RATIONAL;
  }
}
