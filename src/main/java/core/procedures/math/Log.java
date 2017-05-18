package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Log extends AFn {

  /* If number has 307 digits or less, then can use Math.log(double) */
  private static final int MAX_DIGITS = 307;
  private static final double VALUE = Math.log(Math.pow(10, MAX_DIGITS));

  public Log() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "log";
  }

  @Override
  public Number apply1(Object arg) {
    return log((Number)arg);
  }

  public static Number log(Number number) {
    if (number instanceof BigComplex) {
      return ((BigComplex)number).log();
    }
    if (number instanceof Double) {
      return Math.log(number.doubleValue());
    }
    Number n = Utils.INSTANCE.upcast(number);
    if (n instanceof Long) {
      if (n.longValue() == 0) {
        throw new ArithmeticException("log: undefined for 0");
      }
      if (n.longValue() == 1) {
        return 0L;
      }
      return Math.log(n.doubleValue());
    }
    if (number instanceof BigRatio) {
      if (number.equals(BigRatio.ONE)) {
        return 0L;
      }
      return logBig(((BigRatio)number).toBigDecimal());
    }
    if (number instanceof BigDecimal) {
      return logBig((BigDecimal) number);
    }
    if (number instanceof BigInteger) {
      if (((BigInteger)number).signum() == 0) {
        throw new ArithmeticException("log: undefined for 0");
      }
      return logBig(Utils.INSTANCE.toBigDecimal(number));
    }
    return Math.log(number.doubleValue());
  }

  /* Natural logarithm for Big numbers (greater than Double.MAX_VALUE) */
  private static Number logBig(BigDecimal number) {
    if (Double.isFinite(number.doubleValue())) {
      return Math.log(number.doubleValue());
    }
    int digits = integerDigits(number);
    int n = digits / MAX_DIGITS;
    number = number.movePointLeft(n * MAX_DIGITS);
    return (n * VALUE) + Math.log(number.doubleValue());
  }

  /* Return number of digits of a given BigDecimal number */
  private static int integerDigits(BigDecimal n) {
    return n.signum() == 0 ? 1 : n.precision() - n.scale();
  }
}
