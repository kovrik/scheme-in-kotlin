package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Tan extends AFn {

  public Tan() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "tan";
  }

  @Override
  public Number apply1(Object arg) {
    /* Special cases */
    if (NumberUtils.isZero(arg)) {
      return 0L;
    }
    if (arg instanceof BigDecimal) {
      return tan((BigDecimal)arg);
    } else if (arg instanceof BigInteger) {
      return tan((BigInteger)arg);
    } else if (arg instanceof SCMBigComplex) {
      return tan((SCMBigComplex)arg);
    } else if (arg instanceof SCMBigRational) {
      return tan(((SCMBigRational)arg).toBigDecimal());
    }
    return Math.tan(((Number)arg).doubleValue());
  }

  private static double tan(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.tan(v);
    }
  }

  private static double tan(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.tan(v);
    }
  }

  private static SCMBigComplex tan(SCMBigComplex c) {
    SCMBigComplex sin = Sin.sin(c);
    SCMBigComplex cos = Cos.cos(c);
    return sin.divide(cos);
  }
}
