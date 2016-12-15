package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Cos extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cos";
  }

  @Override
  public Number apply(Object... args) {
    return cos((Number)args[0]);
  }

  public static Number cos(Number number) {
    /* Special cases */
    if (NumberUtils.isZero(number)) {
      return 1L;
    }
    if (number instanceof Long) {
      return Math.cos((Long) number);
    } else if (number instanceof Double) {
      return Math.cos((Double) number);
    } else if (number instanceof BigDecimal) {
      return cos((BigDecimal)number);
    } else if (number instanceof SCMBigComplex) {
      // FIXME
      return Cosh.cosh((SCMBigComplex)number);
    } else {
      return cos(((SCMBigRational)number).toBigDecimal());
    }
  }

  public static double cos(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cos(v);
    }
  }
}
