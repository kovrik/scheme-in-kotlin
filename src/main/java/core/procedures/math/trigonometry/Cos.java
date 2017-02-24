package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.math.Multiplication;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

public final class Cos extends AFn {

  public Cos() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Number.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cos";
  }

  @Override
  public Number apply1(Object arg) {
    return cos((Number)arg);
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
      return Cos.cos((SCMBigComplex)number);
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

  public static SCMBigComplex cos(SCMBigComplex c) {
    BigDecimal re = c.getRe();
    BigDecimal im = c.getIm();
    return new SCMBigComplex(Multiplication.apply(Cos.cos(re), Cosh.cosh(im)),
                             Multiplication.apply(-1d, Multiplication.apply(Sin.sin(re), Sinh.sinh(im))));
  }
}
