package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRational;
import core.scm.Type;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Floor extends AFn {

  public Floor() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.Real.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "floor";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof Long || arg instanceof Integer || arg instanceof Short || arg instanceof Byte || arg instanceof BigInteger) {
      return (Number) arg;
    } else if (arg instanceof Double) {
      return Math.floor((Double) arg);
    } else if (arg instanceof Float) {
      return Math.floor((Float) arg);
    } else if (arg instanceof BigDecimal) {
      return ((BigDecimal)arg).setScale(0, BigDecimal.ROUND_DOWN);
    } else if (arg instanceof BigRational) {
      return ((BigRational) arg).floor();
    }
    return Math.floor(((Number)arg).doubleValue());
  }
}
