package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.BigDecimalMath;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public final class Log extends AFn {

  /* If number has 307 digits or less, then can use Math.log(double) */
  private static final int MAX_DIGITS = 307;
  private static final double VALUE = Math.log(Math.pow(10, MAX_DIGITS));

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
    if (number instanceof SCMBigComplex) {
      return ((SCMBigComplex)number).log();
    }
    if (number instanceof Double) {
      if ((Double.isNaN((Double) number)) || (Double.isInfinite((Double) number))) {
        return number;
      }
      return Math.log(number.doubleValue());
    }
    if (number instanceof Long) {
      if (number.longValue() == 0) {
        throw new ArithmeticException("log: undefined for 0");
      }
      return Math.log(number.doubleValue());
    }
    if (number instanceof SCMBigRational) {
      if (((SCMBigRational) number).isZero()){
        throw new ArithmeticException("log: undefined for 0");
      }
      if (number.equals(SCMBigRational.ONE)) {
        return 0L;
      }
      return BigDecimalMath.log((SCMBigRational)number, NumberUtils.DEFAULT_CONTEXT);
    }
    if (number instanceof BigDecimal) {
      if (((BigDecimal)number).compareTo(BigDecimal.ZERO) == 0) {
        throw new ArithmeticException("log: undefined for 0");
      }
      return logBig((BigDecimal) number);
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
