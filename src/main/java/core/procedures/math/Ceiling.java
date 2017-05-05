package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRatio;
import core.scm.Type;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Ceiling extends AFn {

  public Ceiling() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.Real.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "ceiling";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof Long || arg instanceof Integer || arg instanceof Short || arg instanceof Byte || arg instanceof BigInteger) {
      return (Number) arg;
    } else if (arg instanceof Double) {
      return Math.ceil((Double) arg);
    } else if (arg instanceof Float) {
      return Math.ceil((Float) arg);
    } else if (arg instanceof BigDecimal) {
      return ((BigDecimal)arg).setScale(0, BigDecimal.ROUND_UP);
    } else if (arg instanceof BigRatio) {
      return ((BigRatio) arg).ceiling();
    }
    return Math.ceil(((Number)arg).doubleValue());
  }
}
