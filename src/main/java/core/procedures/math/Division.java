package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

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
    if (numerator instanceof BigDecimal) {
      return safeBigDecimalDivision((BigDecimal)numerator, NumberUtils.toBigDecimal(denominator));
    }
    if (denominator instanceof BigDecimal) {
      return safeBigDecimalDivision(NumberUtils.toBigDecimal(numerator), (BigDecimal)denominator);
    }
    double result = numerator.doubleValue() / denominator.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return safeBigDecimalDivision(new BigDecimal(numerator.toString()), new BigDecimal(denominator.toString()));
    }
    return result;
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
