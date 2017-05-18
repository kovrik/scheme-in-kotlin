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
    if (Utils.INSTANCE.isZero(numerator)) {
      return Utils.INSTANCE.inexactnessTaint(numerator, denominator);
    }
    /* Complex numbers*/
    if (numerator instanceof BigComplex) {
      return ((BigComplex) numerator).divide(denominator);
    }
    if (denominator instanceof BigComplex) {
      return new BigComplex(numerator).divide(denominator);
    }
    /* Big Ratio numbers */
    if ((numerator instanceof BigRatio) && (denominator instanceof BigRatio)) {
      return ((BigRatio)numerator).divide((BigRatio)denominator);
    }
    if (numerator instanceof BigRatio) {
      if (Utils.INSTANCE.isExact(denominator)) {
        return ((BigRatio) numerator).divide(Utils.INSTANCE.toBigInteger(denominator));
      } else {
        numerator = numerator.doubleValue();
      }
    }
    if (denominator instanceof BigRatio) {
      if (Utils.INSTANCE.isExact(numerator)) {
        return ((BigRatio)denominator).reciprocal().multiply(Utils.INSTANCE.toBigInteger(numerator));
      } else {
        denominator = denominator.doubleValue();
      }
    }
    if (Utils.INSTANCE.isExact(numerator) && Utils.INSTANCE.isExact(denominator)) {
      return BigRatio.valueOf(Utils.INSTANCE.toBigInteger(numerator), Utils.INSTANCE.toBigInteger(denominator));
    }
    if (numerator instanceof Float && denominator instanceof Float) {
      float result = numerator.floatValue() / denominator.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return Utils.INSTANCE.toBigDecimal(numerator).divide(Utils.INSTANCE.toBigDecimal(denominator), Utils.INSTANCE.getDEFAULT_CONTEXT());
      }
      return result;
    }
    if (numerator instanceof Double || denominator instanceof Double || numerator instanceof Float || denominator instanceof Float) {
      double result = numerator.doubleValue() / denominator.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return Utils.INSTANCE.toBigDecimal(numerator).divide(Utils.INSTANCE.toBigDecimal(denominator), Utils.INSTANCE.getDEFAULT_CONTEXT());
      }
      return result;
    }
    if (numerator instanceof BigDecimal || denominator instanceof BigDecimal) {
      return Utils.INSTANCE.toBigDecimal(numerator).divide(Utils.INSTANCE.toBigDecimal(denominator), Utils.INSTANCE.getDEFAULT_CONTEXT());
    }
    if (numerator instanceof BigInteger || denominator instanceof BigInteger) {
      return Utils.INSTANCE.toBigInteger(numerator).divide(Utils.INSTANCE.toBigInteger(denominator));
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
      return num.divide(den, Utils.INSTANCE.getMathContext(num, den));
    } catch (ArithmeticException e) {
      return num.divide(den, Utils.INSTANCE.getDEFAULT_CONTEXT());
    }
  }
}
