package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Sin extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sin";
  }

  @Override
  public Number apply(Object... args) {
    return sin((Number)args[0]);
  }

  public static Number sin(Number number) {
    /* Special cases */
    if (NumberUtils.isZero(number)) {
      return 0L;
    }
    if (number instanceof Long) {
      return Math.sin((Long) number);
    } else if (number instanceof Double) {
      return Math.sin((Double) number);
    } else if (number instanceof BigDecimal) {
      return sin((BigDecimal)number);
    } else if (number instanceof SCMBigComplex) {
      return Sinh.sinh((SCMBigComplex)number);
    } else {
      return sin(((SCMBigRational)number).toBigDecimal());
    }
  }

  public static double sin(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.sin(v);
    }
  }
}
