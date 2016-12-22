package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public class Tan extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "tan";
  }

  @Override
  public Number apply(Object... args) {
    /* Special cases */
    if (NumberUtils.isZero(args[0])) {
      return 0L;
    }
    if (args[0] instanceof Long) {
      return Math.tan((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.tan((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return tan((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return Tan.tan((SCMBigComplex)args[0]);
    } else {
      return tan(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double tan(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.tan(v);
    }
  }

  public static SCMBigComplex tan(SCMBigComplex c) {
    SCMBigComplex sin = Sin.sin(c);
    SCMBigComplex cos = Cos.cos(c);
    return sin.divide(cos);
  }
}
