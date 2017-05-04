package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Sinh extends AFn {

  public Sinh() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sinh";
  }

  @Override
  public Number apply1(Object arg) {
    /* Special cases */
    if (NumberUtils.isZero(arg)) {
      return 0L;
    }
    if (arg instanceof BigDecimal) {
      return sinh((BigDecimal)arg);
    } else if (arg instanceof BigInteger) {
      return sinh((BigInteger)arg);
    } else if (arg instanceof SCMBigComplex) {
      return sinh((SCMBigComplex)arg);
    } else if (arg instanceof SCMBigRational) {
      return sinh(((SCMBigRational)arg).toBigDecimal());
    }
    return Math.sinh(((Number)arg).doubleValue());
  }

  static double sinh(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.sinh(v);
    }
  }

  static double sinh(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.sinh(v);
    }
  }

  /* sinh(x + yi) = sinh(x)*cos(y) + cosh(x)*sin(y)*i */
  static Number sinh(SCMBigComplex c) {
    BigDecimal x = c.getRe();
    BigDecimal y = c.getIm();
    double re = sinh(x) * Cos.cos(y);
    double im = Cosh.cosh(x) * Sin.sin(y);
    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new SCMBigComplex(re, im);
  }
}
