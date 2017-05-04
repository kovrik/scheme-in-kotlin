package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.scm.SCMClass;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

public final class Round extends AFn {

  public Round() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMClass.Real.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "round";
  }

  @Override
  public Number apply1(Object arg) {
    return round((Number) arg);
  }

  private Number round(Number number) {
    if (number instanceof Long || number instanceof Integer || number instanceof Short || number instanceof Byte || number instanceof BigInteger) {
      return number;
    } else if (number instanceof BigDecimal) {
      BigDecimal bd = (BigDecimal) number;
      if (bd.scale() == 0) {
        return bd.round(MathContext.UNLIMITED);
      } else {
        return bd.round(NumberUtils.DEFAULT_CONTEXT);
      }
    } else if (number instanceof SCMBigRational) {
      return ((SCMBigRational) number).round();
    }
    return Math.rint(number.doubleValue());
  }
}
