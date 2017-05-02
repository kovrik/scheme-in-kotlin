package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Truncate extends AFn {

  public Truncate() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMClass.Real.class}));
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
    } else if (arg instanceof SCMBigRational){
      return ((SCMBigRational) arg).truncate();
    }
    return (Number)arg;
  }
}
