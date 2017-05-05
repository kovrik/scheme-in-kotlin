package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Quotient extends AFn {

  public Quotient() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{Long.class, Long.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "quotient";
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    /* Special cases */
    if (Utils.isOne(arg2)) {
      return Utils.inexactnessTaint((Number)arg1, (Number) arg2);
    }
    if (Utils.isZero(arg2)) {
      throw new ArithmeticException(String.format("%s: undefined for 0", getName()));
    }
    return apply((Number) arg1, (Number) arg2);
  }

  private Number apply(BigDecimal first, BigDecimal second) {
    int scale = Math.max(first.scale(), second.scale());
    if (scale > 0) {
      return first.divide(second, Utils.DEFAULT_CONTEXT).setScale(0, Utils.ROUNDING_MODE)
                  .setScale(1, Utils.ROUNDING_MODE);
    } else {
      return first.divideToIntegralValue(second).setScale(scale, Utils.ROUNDING_MODE);
    }
  }

  private Number apply(BigInteger first, BigInteger second) {
    return first.divide(second);
  }

  private Number apply(Number first, Number second) {
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return apply((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return apply((BigDecimal)first, Utils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return apply(Utils.toBigDecimal(first), (BigDecimal)second);
    }
    if ((first instanceof BigInteger) && (second instanceof BigInteger)) {
      return apply((BigInteger)first, (BigInteger)second);
    }
    if (first instanceof BigInteger) {
      return apply(Utils.toBigInteger(first), Utils.toBigInteger(second));
    }
    if (second instanceof BigInteger) {
      return apply(Utils.toBigInteger(first), Utils.toBigInteger(second));
    }
    if (((first instanceof Double) || (second instanceof Double) || (first instanceof Float) || (second instanceof Float)) &&
        (Utils.isInteger(first)) && Utils.isInteger(second)) {

      return Long.valueOf(first.longValue() / second.longValue()).doubleValue();
    }

    if ((first instanceof Double) || (second instanceof Double) ||
        (first instanceof BigRatio) || (second instanceof BigRatio)) {

      return apply(Utils.toBigDecimal(first), Utils.toBigDecimal(second));
    }
    return first.longValue() / second.longValue();
  }
}
