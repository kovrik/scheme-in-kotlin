package core.procedures.math;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

@FnArgs(restArgsType = Number.class)
public final class Division extends AFn {

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
    if (args == null || args.length == 0) {
      throw new ArityException(0, getName());
    }
    Number result;
    if (args.length == 1) {
      return apply(1L, (Number)args[0]);
    } else {
      result = (Number)args[0];
    }
    for (int d = 1; d <= args.length - 1; d++) {
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
        return ((SCMBigRational) numerator).divide(new SCMBigRational(new BigInteger(denominator.toString()), BigInteger.ONE));
      } else {
        numerator = ((SCMBigRational) numerator).doubleOrBigDecimalValue();
      }
    }
    if (denominator instanceof SCMBigRational) {
      if (NumberUtils.isExact(numerator)) {
        return (new SCMBigRational(new BigInteger(numerator.toString()), BigInteger.ONE).divide((SCMBigRational) denominator));
      } else {
        denominator = ((SCMBigRational) denominator).doubleOrBigDecimalValue();
      }
    }

    if (NumberUtils.isExact(numerator) &&
        NumberUtils.isExact(denominator)) {

      return new SCMBigRational(new BigInteger(numerator.toString()), new BigInteger(denominator.toString()));
    }
    if (numerator instanceof BigDecimal) {
      return safeBigDecimalDivision((BigDecimal)numerator, new BigDecimal(denominator.toString()));
    }
    if (denominator instanceof BigDecimal) {
      return safeBigDecimalDivision(new BigDecimal(numerator.toString()), (BigDecimal)denominator);
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
  // FIXME Performance
  public static BigDecimal safeBigDecimalDivision(BigDecimal num, BigDecimal den) {
    try {
      return num.divide(den, NumberUtils.getMathContext(num, den));
    } catch (ArithmeticException e) {
      return num.divide(den, NumberUtils.DEFAULT_CONTEXT);
    }
  }
}
