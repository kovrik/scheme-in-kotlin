package core.scm;

import core.procedures.math.Division;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.Map;

/**
 * TODO Create Ratio class for small ratios (with longs for num and den)
 */
public final class BigRatio extends Number implements Comparable<BigRatio> {

  public static final BigRatio ZERO = new BigRatio(BigInteger.ZERO);
  public static final BigRatio ONE  = new BigRatio(BigInteger.ONE);

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

  public static BigRatio valueOf(String numerator, String denominator) {
    return valueOf(parseBigInteger(numerator), parseBigInteger(denominator));
  }

  public static BigRatio valueOf(BigInteger numerator, BigInteger denominator) {
    if (BigInteger.ZERO.equals(denominator)) {
      throw new ArithmeticException("/ by zero");
    }
    if (BigInteger.ZERO.equals(numerator)) {
      return ZERO;
    }
    if (BigInteger.ONE.equals(numerator) && BigInteger.ONE.equals(denominator)) {
      return ONE;
    }
    if (BigInteger.ONE.equals(denominator)) {
      return new BigRatio(numerator);
    }
    return new BigRatio(numerator, denominator);
  }

  private BigRatio(BigInteger numerator) {
    this.numerator = numerator;
    this.denominator = BigInteger.ONE;
  }

  private BigRatio(BigInteger numerator, BigInteger denominator) {
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
    return CONSTANTS.getOrDefault(number, new BigInteger(number));
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
    return bigDecimal.setScale(scale, Utils.ROUNDING_MODE);
  }

  public boolean isDenominatorEqualToOne() {
    return BigInteger.ONE.compareTo(denominator) == 0;
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

  public BigRatio abs() {
    return new BigRatio(numerator.abs(), denominator.abs());
  }

  public BigRatio ceiling() {
    int round = isPositive() ? BigDecimal.ROUND_UP : BigDecimal.ROUND_DOWN;
    return new BigRatio(new BigDecimal(numerator).divide(new BigDecimal(denominator), round).toBigInteger());
  }

  public BigRatio floor() {
    int round = isPositive() ? BigDecimal.ROUND_DOWN : BigDecimal.ROUND_UP;
    return new BigRatio(new BigDecimal(numerator).divide(new BigDecimal(denominator), round).toBigInteger());
  }

  public BigRatio round() {
    BigDecimal number = toBigDecimal();
    BigDecimal round = number.setScale(0, Utils.ROUNDING_MODE);
    return new BigRatio(round.toBigInteger());
  }

  public BigRatio truncate() {
    return isNegative() ? ceiling() : floor();
  }

  @Override
  public String toString() {
    return (denominator.equals(BigInteger.ONE)) ? numerator.toString() : numerator + "/" + denominator;
  }

  @Override
  public int compareTo(BigRatio other) {
    return this.numerator.multiply(other.denominator).compareTo(this.denominator.multiply(other.numerator));
  }

  @Override
  public boolean equals(Object y) {
    if (y == this) return true;
    if (y == null) return false;
    if (y.getClass() != this.getClass()) return false;
    BigRatio b = (BigRatio) y;
    return compareTo(b) == 0;
  }

  @Override
  public int hashCode() {
    return this.toString().hashCode();
  }

  public BigRatio multiply(BigRatio other) {
    return new BigRatio(this.numerator.multiply(other.numerator), this.denominator.multiply(other.denominator));
  }

  public BigRatio multiply(BigInteger other) {
    return new BigRatio(this.numerator.multiply(other), this.denominator);
  }

  public BigRatio plus(BigRatio other) {
    BigInteger numerator   = this.numerator.multiply(other.denominator).add(other.numerator.multiply(this.denominator));
    BigInteger denominator = this.denominator.multiply(other.denominator);
    return new BigRatio(numerator, denominator);
  }

  public BigRatio negate() {
    return new BigRatio(numerator.negate(), denominator);
  }

  public BigRatio minus(BigRatio other) {
    return this.plus(other.negate());
  }

  public BigRatio reciprocal() {
    return new BigRatio(denominator, numerator);
  }

  public BigRatio divide(BigRatio other) {
    return this.multiply(other.reciprocal());
  }

  public BigRatio divide(BigInteger other) {
    return new BigRatio(this.numerator, this.denominator.multiply(other));
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

  public int signum() {
    return numerator.signum() * denominator.signum();
  }
}
