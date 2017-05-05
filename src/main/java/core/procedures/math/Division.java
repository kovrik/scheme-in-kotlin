package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Division extends AFn {

  public Division() {
    super(new FnArgsBuilder().min(1).rest(Number.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "/";
  }

  @Override
  public Number apply(Object... args) {
    if (args.length == 1) {
      return apply(1L, (Number) args[0]);
    }
    Number result = (Number)args[0];
    for (int d = 1; d < args.length; d++) {
      result = apply(result, (Number)args[d]);
    }
    return result;
  }

  private Number apply(Number numerator, Number denominator) {
    /* Complex numbers*/
    if (numerator instanceof BigComplex) {
      return ((BigComplex) numerator).divide(denominator);
    }
    if (denominator instanceof BigComplex) {
      return new BigComplex(numerator).divide(denominator);
    }
    /* Big Rational numbers */
    if ((numerator instanceof BigRatio) && (denominator instanceof BigRatio)) {
      return ((BigRatio)numerator).divide((BigRatio)denominator);
    }
    if (numerator instanceof BigRatio) {
      if (Utils.isExact(denominator)) {
        return ((BigRatio) numerator).divide(BigRatio.valueOf(denominator.toString(), "1"));
      } else {
        numerator = ((BigRatio) numerator).doubleOrBigDecimalValue();
      }
    }
    if (denominator instanceof BigRatio) {
      if (Utils.isExact(numerator)) {
        return (BigRatio.valueOf(numerator.toString(), "1").divide((BigRatio) denominator));
      } else {
        denominator = ((BigRatio) denominator).doubleOrBigDecimalValue();
      }
    }
    if (Utils.isExact(numerator) &&
        Utils.isExact(denominator)) {

      return BigRatio.valueOf(numerator.toString(), denominator.toString());
    }
    if (numerator instanceof Float && denominator instanceof Float) {
      float result = numerator.floatValue() / denominator.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return new BigDecimal(numerator.toString()).divide(new BigDecimal(denominator.toString()), Utils.DEFAULT_CONTEXT);
      }
      return result;
    }
    if (numerator instanceof Double || denominator instanceof Double || numerator instanceof Float || denominator instanceof Float) {
      double result = numerator.doubleValue() / denominator.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return new BigDecimal(numerator.toString()).divide(new BigDecimal(denominator.toString()), Utils.DEFAULT_CONTEXT);
      }
      return result;
    }
    if (numerator instanceof BigDecimal) {
      return ((BigDecimal)numerator).divide(Utils.toBigDecimal(denominator), Utils.DEFAULT_CONTEXT);
    }
    if (denominator instanceof BigDecimal) {
      return ((BigDecimal) denominator).divide(Utils.toBigDecimal(numerator), Utils.DEFAULT_CONTEXT);
    }
    if (numerator instanceof BigInteger) {
      return ((BigInteger)numerator).divide(Utils.toBigInteger(denominator));
    }
    if (denominator instanceof BigInteger) {
      return (Utils.toBigInteger(numerator)).divide((BigInteger)denominator);
    }
    double f = numerator.doubleValue();
    double s = denominator.doubleValue();
    return f / s;
  }

  /**
   * Rolls back to DEFAULT_CONTEXT if result cannot be represented with UNLIMITED precision
   */
  public static BigDecimal safeBigDecimalDivision(BigDecimal num, BigDecimal den) {
    try {
      return num.divide(den, Utils.getMathContext(num, den));
    } catch (ArithmeticException e) {
      return num.divide(den, Utils.DEFAULT_CONTEXT);
    }
  }
}
