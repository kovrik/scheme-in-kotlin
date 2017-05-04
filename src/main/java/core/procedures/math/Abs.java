package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRational;
import core.scm.Type;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Abs extends AFn {

  public Abs() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.Real.class}).build());
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
    } else if (number instanceof Integer) {
      return Math.abs((Integer) number);
    } else if (number instanceof Double) {
      return Math.abs((Double) number);
    } else if (number instanceof Float) {
      return Math.abs((Float) number);
    } else if (number instanceof BigInteger) {
      return ((BigInteger) number).abs();
    } else if (number instanceof BigDecimal) {
      return ((BigDecimal) number).abs();
    } else if (number instanceof BigRational) {
      return ((BigRational) number).abs();
    }
    return Math.abs(number.longValue());
  }
}
