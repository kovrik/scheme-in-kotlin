package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRational;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Denominator extends AFn {

  public Denominator() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{BigRational.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "denominator";
  }

  @Override
  public Number apply1(Object arg) {
    return denominator(arg);
  }

  private Number denominator(Object o) {
    boolean isExact = Utils.isExact(o);
    Number exact;
    if (isExact) {
      exact = (Number)o;
    } else {
      exact = ToExact.toExact(o);
    }
    if (exact instanceof BigRational) {
      if (!isExact) {
        BigDecimal result = new BigDecimal(((BigRational) exact).getDenominator());
        return result.setScale(1, Utils.ROUNDING_MODE);
      }
      return ((BigRational) exact).getDenominator();
    }
    if (exact instanceof Long || exact instanceof Integer || exact instanceof Byte || exact instanceof Short) {
      return 1L;
    }
    if (exact instanceof Double || exact instanceof Float) {
      return 1d;
    }
    if (exact instanceof BigInteger) {
      return BigInteger.ONE;
    }
    if (exact instanceof BigDecimal) {
      if (((BigDecimal) exact).scale() == 0) {
        return BigDecimal.ONE;
      } else {
        return BigDecimal.ONE.setScale(1, Utils.ROUNDING_MODE);
      }
    }
    return 1L;
  }
}
