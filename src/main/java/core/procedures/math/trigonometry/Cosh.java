package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Cosh extends AFn {

  public Cosh() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Number.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cosh";
  }

  @Override
  public Number apply1(Object arg) {
    /* Special cases */
    if (NumberUtils.isZero(arg)) {
      return 1L;
    }
    if (arg instanceof BigDecimal) {
      return cosh((BigDecimal) arg);
    } else if (arg instanceof BigInteger) {
      return cosh((BigInteger) arg);
    } else if (arg instanceof SCMBigComplex) {
      return cosh((SCMBigComplex)arg);
    } else if (arg instanceof SCMBigRational){
      return cosh(((SCMBigRational)arg).toBigDecimal());
    }
    return Math.cosh(((Number)arg).doubleValue());
  }

  static double cosh(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cosh(v);
    }
  }

  static double cosh(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cosh(v);
    }
  }

  /* cosh(x + yi) = cosh(x)*cos(y) + sinh(x)*sin(y)*i */
  static Number cosh(SCMBigComplex c) {
    BigDecimal x = c.getRe();
    BigDecimal y = c.getIm();
    double re = Cosh.cosh(x) * Cos.cos(y);
    double im = Sinh.sinh(x) * Sin.sin(y);
    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new SCMBigComplex(re, im);
  }
}
