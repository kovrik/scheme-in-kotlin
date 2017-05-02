package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Tanh extends AFn {

  public Tanh() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Number.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "tanh";
  }

  @Override
  public Number apply1(Object arg) {
    /* Special cases */
    if (NumberUtils.isZero(arg)) {
      return 0L;
    }
    if (arg instanceof BigDecimal) {
      return tanh((BigDecimal)arg);
    } else if (arg instanceof BigInteger) {
      return tanh((BigInteger)arg);
    } else if (arg instanceof SCMBigComplex) {
      return tanh((SCMBigComplex)arg);
    } else if (arg instanceof SCMBigRational) {
      return tanh(((SCMBigRational)arg).toBigDecimal());
    }
    return Math.tanh(((Number)arg).doubleValue());
  }

  private static double tanh(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.tanh(v);
    }
  }

  private static double tanh(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.tanh(v);
    }
  }

  private static Number tanh(SCMBigComplex c) {
    Number sinh = Sinh.sinh(c);
    if ((sinh instanceof Double) && (Double.isInfinite((Double)sinh) || Double.isNaN((Double)sinh))) {
      return Double.NaN;
    }
    Number cosh = Cosh.cosh(c);
    if ((cosh instanceof Double) && (Double.isInfinite((Double)cosh) || Double.isNaN((Double)cosh))) {
      return Double.NaN;
    }
    return ((SCMBigComplex)sinh).divide(cosh);
  }
}
