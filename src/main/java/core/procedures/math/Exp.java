package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRational;

import static core.utils.Utils.E;

public final class Exp extends AFn {

  public Exp() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "exp";
  }

  @Override
  public Number apply1(Object arg) {
    return exp((Number)arg);
  }

  public static Number exp(Number number) {
    if (number instanceof Double) {
      if ((Double)number == Double.NEGATIVE_INFINITY) {
        return 0L;
      }
      if ((Double.isNaN((Double) number)) || (Double.isInfinite((Double) number))) {
        return number;
      }
      return Math.exp(number.doubleValue());
    }
    if (number instanceof Float) {
      if ((Float)number == Float.NEGATIVE_INFINITY) {
        return 0L;
      }
      if ((Float.isNaN((Float) number)) || (Float.isInfinite((Float) number))) {
        return number;
      }
      return Math.exp(number.doubleValue());
    }
    if (number instanceof Long || number instanceof Byte || number instanceof Short || number instanceof Integer) {
      if (number.longValue() == 0) {
        return 1L;
      }
      return Math.exp(number.doubleValue());
    }
    if (number instanceof BigRational) {
      /* Special cases */
      if (((BigRational) number).isZero()) {
        return 1L;
      }
      if (((BigRational) number).isOne()) {
        return Math.exp(1d);
      }
    }
    return Expt.expt(E, number);
  }
}
