package core.procedures.math;

import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

public final class Abs extends AFn {

  public Abs() {
    super(1, 1, new Class[]{SCMClass.Real.class});
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "abs";
  }

  @Override
  public Number apply1(Object arg) {
    return abs((Number)arg);
  }

  public static Number abs(Number number) {
    if (number instanceof Long) {
      return Math.abs((Long) number);
    } else if (number instanceof Double) {
      return Math.abs((Double) number);
    } else if (number instanceof BigDecimal) {
      return ((BigDecimal) number).abs();
    } else {
      return ((SCMBigRational) number).abs();
    }
  }
}
