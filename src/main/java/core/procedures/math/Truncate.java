package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRational;
import core.scm.Type;

import java.math.BigDecimal;

public final class Truncate extends AFn {

  public Truncate() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.Real.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "truncate";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof Double || arg instanceof Float) {
      if (((Number)arg).doubleValue() < 0) {
        return Math.ceil(((Number)arg).doubleValue());
      } else {
        return Math.floor(((Number)arg).doubleValue());
      }
    } else if (arg instanceof BigDecimal) {
      BigDecimal bd = (BigDecimal) arg;
      if (bd.signum() < 0) {
        return bd.setScale(0, BigDecimal.ROUND_UP);
      } else {
        return bd.setScale(0, BigDecimal.ROUND_DOWN);
      }
    } else if (arg instanceof BigRational){
      return ((BigRational) arg).truncate();
    }
    return (Number)arg;
  }
}
