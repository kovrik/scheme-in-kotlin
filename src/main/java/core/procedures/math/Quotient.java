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
    if (Utils.INSTANCE.isOne(arg2)) {
      return Utils.INSTANCE.inexactnessTaint((Number)arg1, (Number) arg2);
    }
    if (Utils.INSTANCE.isZero(arg2)) {
      throw new ArithmeticException("quotient: undefined for 0");
    }
    return apply((Number) arg1, (Number) arg2);
  }

  private Number apply(BigDecimal first, BigDecimal second) {
    int scale = Math.max(first.scale(), second.scale());
    if (scale > 0) {
      return first.divide(second, Utils.INSTANCE.getDEFAULT_CONTEXT()).setScale(0, Utils.INSTANCE.getROUNDING_MODE())
                  .setScale(1, Utils.INSTANCE.getROUNDING_MODE());
    } else {
      return first.divideToIntegralValue(second).setScale(scale, Utils.INSTANCE.getROUNDING_MODE());
    }
  }

  private Number apply(BigInteger first, BigInteger second) {
    return first.divide(second);
  }

  private Number apply(Number first, Number second) {
    if (Utils.INSTANCE.isZero(first)) {
      return Utils.INSTANCE.inexactnessTaint(first, second);
    }
    if ((first instanceof BigDecimal) || (second instanceof BigDecimal)) {
      return apply(Utils.INSTANCE.toBigDecimal(first), Utils.INSTANCE.toBigDecimal(second));
    }
    if ((first instanceof BigInteger) || (second instanceof BigInteger)) {
      return apply(Utils.INSTANCE.toBigInteger(first), Utils.INSTANCE.toBigInteger(second));
    }
    if (((first instanceof Double) || (second instanceof Double) || (first instanceof Float) || (second instanceof Float)) &&
        (Utils.INSTANCE.isInteger(first)) && Utils.INSTANCE.isInteger(second)) {

      return Long.valueOf(first.longValue() / second.longValue()).doubleValue();
    }
    if ((first instanceof Double) || (second instanceof Double) ||
        (first instanceof BigRatio) || (second instanceof BigRatio)) {

      return apply(Utils.INSTANCE.toBigDecimal(first), Utils.INSTANCE.toBigDecimal(second));
    }
    return first.longValue() / second.longValue();
  }
}
