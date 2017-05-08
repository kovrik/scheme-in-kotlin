package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRatio;
import core.scm.Type;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

public final class Round extends AFn {

  public Round() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.Real.class}).build());
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
      return bd.scale() == 0 ? bd.round(MathContext.UNLIMITED) : bd.round(Utils.DEFAULT_CONTEXT);
    } else if (number instanceof BigRatio) {
      return ((BigRatio) number).round();
    }
    return Math.rint(number.doubleValue());
  }
}
