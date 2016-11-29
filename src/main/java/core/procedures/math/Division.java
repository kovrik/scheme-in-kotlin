package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Division extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "/";
  }

  @Override
  public Number invoke(Object... args) {
    if (args == null || args.length == 0) {
      throw new ArityException(0, getName());
    }
    if (!(args[0] instanceof Number)) {
      throw new WrongTypeException("Number", args[0]);
    }
    Number result;
    if (args.length == 1) {
      return invoke((Number)1L, (Number)args[0]);
    } else {
      result = (Number)args[0];
    }
    for (int d = 1; d <= args.length - 1; d++) {
      if (!(args[d] instanceof Number)) {
        throw new WrongTypeException("Number", args[d]);
      }
      result = invoke((Number)result, (Number)args[d]);
    }
    return result;
  }

  public Number invoke(Number numerator, Number denominator) {
    /* Big Rational numbers */
    if ((numerator instanceof SCMBigRational) && (denominator instanceof SCMBigRational)) {
      return ((SCMBigRational)numerator).divide((SCMBigRational)denominator);
    }
    if (numerator instanceof SCMBigRational) {
      if (denominator instanceof Long) {
        return ((SCMBigRational) numerator).divide(new SCMBigRational(new BigInteger(denominator.toString()), BigInteger.ONE));
      } else {
        numerator = ((SCMBigRational) numerator).doubleOrBigDecimalValue();
      }
    }
    if (denominator instanceof SCMBigRational) {
      if (numerator instanceof Long) {
        return ((SCMBigRational) denominator).divide(new SCMBigRational(new BigInteger(numerator.toString()), BigInteger.ONE));
      } else {
        denominator = ((SCMBigRational) denominator).doubleOrBigDecimalValue();
      }
    }

    if ((numerator instanceof Long) &&
        (denominator instanceof Long)) {

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
