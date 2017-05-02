package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Division extends AFn {

  public Division() {
    super(new FnArgsBuilder().minArgs(1).restArgsType(Number.class));
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
    if (numerator instanceof SCMBigComplex) {
      return ((SCMBigComplex) numerator).divide(denominator);
    }
    if (denominator instanceof SCMBigComplex) {
      return new SCMBigComplex(numerator).divide(denominator);
    }
    /* Big Rational numbers */
    if ((numerator instanceof SCMBigRational) && (denominator instanceof SCMBigRational)) {
      return ((SCMBigRational)numerator).divide((SCMBigRational)denominator);
    }
    if (numerator instanceof SCMBigRational) {
      if (NumberUtils.isExact(denominator)) {
        return ((SCMBigRational) numerator).divide(SCMBigRational.valueOf(denominator.toString(), "1"));
      } else {
        numerator = ((SCMBigRational) numerator).doubleOrBigDecimalValue();
      }
    }
    if (denominator instanceof SCMBigRational) {
      if (NumberUtils.isExact(numerator)) {
        return (SCMBigRational.valueOf(numerator.toString(), "1").divide((SCMBigRational) denominator));
      } else {
        denominator = ((SCMBigRational) denominator).doubleOrBigDecimalValue();
      }
    }
    if (NumberUtils.isExact(numerator) &&
        NumberUtils.isExact(denominator)) {

      return SCMBigRational.valueOf(numerator.toString(), denominator.toString());
    }
    if (numerator instanceof Float && denominator instanceof Float) {
      float result = numerator.floatValue() / denominator.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return new BigDecimal(numerator.toString()).divide(new BigDecimal(denominator.toString()), NumberUtils.DEFAULT_CONTEXT);
      }
      return result;
    }
    if (numerator instanceof Double || denominator instanceof Double || numerator instanceof Float || denominator instanceof Float) {
      double result = numerator.doubleValue() / denominator.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return new BigDecimal(numerator.toString()).divide(new BigDecimal(denominator.toString()), NumberUtils.DEFAULT_CONTEXT);
      }
      return result;
    }
    if (numerator instanceof BigDecimal) {
      return ((BigDecimal)numerator).divide(NumberUtils.toBigDecimal(denominator), NumberUtils.DEFAULT_CONTEXT);
    }
    if (denominator instanceof BigDecimal) {
      return ((BigDecimal) denominator).divide(NumberUtils.toBigDecimal(numerator), NumberUtils.DEFAULT_CONTEXT);
    }
    if (numerator instanceof BigInteger) {
      return ((BigInteger)numerator).divide(new BigInteger(denominator.toString()));
    }
    if (denominator instanceof BigInteger) {
      return ((BigInteger)denominator).divide(new BigInteger(numerator.toString()));
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
      return num.divide(den, NumberUtils.getMathContext(num, den));
    } catch (ArithmeticException e) {
      return num.divide(den, NumberUtils.DEFAULT_CONTEXT);
    }
  }
}
