package core.scm;

import core.procedures.math.Division;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.Map;

/**
 * TODO Create SCMRational class for small rational numbers
 */
public class SCMBigRational extends Number implements ISCMClass, Comparable<SCMBigRational> {

  public static final SCMBigRational ZERO  = new SCMBigRational(BigInteger.ZERO, BigInteger.ONE);
  public static final SCMBigRational ONE   = new SCMBigRational(BigInteger.ONE,  BigInteger.ONE);

  private static final Map<String, BigInteger> CONSTANTS = new HashMap<>();
  static {
    CONSTANTS.put("-2", new BigInteger("-2").negate());
    CONSTANTS.put("-1", BigInteger.ONE.negate());
    CONSTANTS.put("0",  BigInteger.ZERO);
    CONSTANTS.put("1",  BigInteger.ONE);
    CONSTANTS.put("2",  new BigInteger("2"));
    CONSTANTS.put("10", BigInteger.TEN);
  }

  private final BigInteger numerator;
  private final BigInteger denominator;

  public static SCMBigRational valueOf(String numerator, String denominator) {
    BigInteger den = parseBigInteger(denominator);
    if (BigInteger.ZERO.equals(den)) {
      throw new ArithmeticException("/ by zero");
    }
    BigInteger num = parseBigInteger(numerator);
    if (BigInteger.ZERO.equals(num)) {
      return ZERO;
    }
    if (BigInteger.ONE.equals(num) && BigInteger.ONE.equals(den)) {
      return ONE;
    }
    return new SCMBigRational(num, den);
  }

  public SCMBigRational(BigInteger numerator, BigInteger denominator) {
    if (BigInteger.ZERO.equals(denominator)) {
      throw new ArithmeticException("/ by zero");
    }
    // reduce fraction
    BigInteger g = numerator.gcd(denominator);
    BigInteger den = denominator.divide(g);
    // to ensure invariant that denominator is positive
    if (den.signum() < 0) {
      this.numerator = numerator.negate();
      this.denominator = den.negate();
    } else {
      this.numerator = numerator.divide(g);
      this.denominator = den;
    }
  }

  private static BigInteger parseBigInteger(String number) {
    BigInteger bigInteger = CONSTANTS.get(number);
    return bigInteger == null ? new BigInteger(number) : bigInteger;
  }

  public BigInteger getNumerator() {
    return numerator;
  }

  public BigInteger getDenominator() {
    return denominator;
  }

  public BigDecimal toBigDecimal() {
    return Division.safeBigDecimalDivision(new BigDecimal(numerator), new BigDecimal(denominator));
  }

  public BigDecimal toBigDecimalInexact() {
    BigDecimal bigDecimal = Division.safeBigDecimalDivision(new BigDecimal(numerator), new BigDecimal(denominator));
    int scale = Math.max(1, bigDecimal.scale());
    return bigDecimal.setScale(scale, NumberUtils.ROUNDING_MODE);
  }

  public boolean isDenominatorEqualToOne() {
    return denominator.compareTo(BigInteger.ONE) == 0;
  }

  public boolean isZero() {
    return signum() == 0;
  }

  public boolean isOne() {
    return compareTo(ONE) == 0;
  }

  public boolean isPositive() {
    return signum() == 1;
  }

  public boolean isNegative() {
    return signum() == -1;
  }

  public SCMBigRational abs() {
    return new SCMBigRational(numerator.abs(), denominator.abs());
  }

  public SCMBigRational ceiling() {
    int round = isPositive() ? BigDecimal.ROUND_UP : BigDecimal.ROUND_DOWN;
    return new SCMBigRational(new BigDecimal(numerator).divide(new BigDecimal(denominator), round).toBigInteger(), BigInteger.ONE);
  }

  public SCMBigRational floor() {
    int round = isPositive() ? BigDecimal.ROUND_DOWN : BigDecimal.ROUND_UP;
    return new SCMBigRational(new BigDecimal(numerator).divide(new BigDecimal(denominator), round).toBigInteger(), BigInteger.ONE);
  }

  public SCMBigRational round() {
    BigDecimal number = toBigDecimal();
    BigDecimal round = (number.scale() == 0) ? number.round(MathContext.UNLIMITED) : number.round(NumberUtils.DEFAULT_CONTEXT);
    return new SCMBigRational(round.toBigInteger(), BigInteger.ONE);
  }

  public SCMBigRational truncate() {
    return isNegative() ? ceiling() : floor();
  }

  @Override
  public String toString() {
    return (denominator.equals(BigInteger.ONE)) ? numerator.toString() : numerator + "/" + denominator;
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

  private SCMBigRational reciprocal() {
    return new SCMBigRational(denominator, numerator);
  }

  public SCMBigRational divide(SCMBigRational other) {
    return this.multiply(other.reciprocal());
  }

  private BigDecimal quotient() {
    BigDecimal numerator   = new BigDecimal(this.numerator);
    BigDecimal denominator = new BigDecimal(this.denominator);
    return numerator.divide(denominator, 32, RoundingMode.HALF_EVEN);
  }

  @Override
  public int intValue() {
    return quotient().intValue();
  }

  @Override
  public long longValue() {
    return quotient().longValue();
  }

  @Override
  public float floatValue() {
    return quotient().floatValue();
  }

  @Override
  public double doubleValue() {
    return quotient().doubleValue();
  }

  public Number doubleOrBigDecimalValue() {
    double doubleValue = doubleValue();
    if (!isZero() && Double.compare(doubleValue, 0d) == 0) {
      return toBigDecimal();
    }
    return doubleValue;
  }

  public int signum() {
    return numerator.signum() * denominator.signum();
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.RATIONAL;
  }
}
