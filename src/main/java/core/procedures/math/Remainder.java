package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Remainder extends AFn {

  private static final String NAME = "remainder";

  public Remainder() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{Long.class, Long.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    return apply((Number)arg1, (Number)arg2);
  }

  private static Number apply(BigDecimal first, BigDecimal second) {
    return first.remainder(second);
  }

  private static Number apply(BigInteger first, BigInteger second) {
    return first.remainder(second);
  }

  public static Number apply(Number first, Number second) {
    if (Utils.isZero(second)) {
      throw new ArithmeticException("remainder: undefined for 0");
    }
    if (Utils.isZero(first)) {
      return Utils.inexactnessTaint(first, second);
    }
    if (first instanceof BigRatio || second instanceof BigRatio) {
      return apply(Utils.toBigDecimal(first), Utils.toBigDecimal(second));
    }
    if ((first instanceof BigDecimal) || (second instanceof BigDecimal)) {
      return apply(Utils.toBigDecimal(first), Utils.toBigDecimal(second));
    }
    if ((first instanceof BigInteger) || (second instanceof BigInteger)) {
      return apply(Utils.toBigInteger(first), Utils.toBigInteger(second));
    }
    if ((first instanceof Double) || (second instanceof Double) || (first instanceof Float) || (second instanceof Float)) {
      double result = first.doubleValue() % second.doubleValue();
      // Don't want negative zero
      if (result == -0.0) {
        return Math.abs(result);
      }
      return result;
    }
    return first.longValue() % second.longValue();
  }
}
