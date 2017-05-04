package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;

public final class Sqrt extends AFn {

  public Sqrt() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sqrt";
  }

  @Override
  public Number apply1(Object arg) {
    return sqrt((Number) arg);
  }

  public static Number sqrt(Number number) {
    if (number instanceof BigComplex) {
      if (((BigComplex)number).getIm().signum()== 0) {
        return sqrt(((BigComplex)number).getRe());
      }
      return ((BigComplex)number).sqrt();
    }
    return Math.sqrt(number.doubleValue());
  }
}
