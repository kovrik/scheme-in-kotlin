package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Acos extends AFn {

  public Acos() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "acos";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof BigDecimal) {
      return acos((BigDecimal)arg);
    } else if (arg instanceof BigInteger) {
      return acos((BigInteger)arg);
    } else if (arg instanceof BigComplex) {
      return acos((BigComplex)arg);
    } else if (arg instanceof BigRatio) {
      return acos(((BigRatio)arg).toBigDecimal());
    }
    double acos = Math.acos(((Number)arg).doubleValue());
    if (Double.isNaN(acos)) {
      return acos(new BigComplex((Number)arg));
    }
    return acos;
  }

  private static Number acos(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      double acos = Math.acos(v);
      if (Double.isNaN(acos)) {
        return acos(new BigComplex(bd));
      }
      return acos;
    }
  }

  private static Number acos(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      double acos = Math.acos(v);
      if (Double.isNaN(acos)) {
        return acos(new BigComplex(bi));
      }
      return acos;
    }
  }

  /* acos(a+bi) = acos(A) - ln(B + sqrt(B*B - 1))*i
   *
   * A = (sqrt((1+a)^2 + b^2) - sqrt((1-a)^2 + b^2))/2
   * B = (sqrt((1+a)^2 + b^2) + sqrt((1-a)^2 + b^2))/2
   *
   **/
  private static Number acos(BigComplex c) {
    BigDecimal r = c.getRe();
    BigDecimal i = c.getIm();
    int signum;
    if (i.signum() == 0) {
      signum = r.signum();
    } else {
      signum = -i.signum();
    }
    double a = r.doubleValue();
    if (Double.isInfinite(a) || Double.isNaN(a)) {
      return Double.NaN;
    }
    double b = i.doubleValue();
    if (Double.isInfinite(b) || Double.isNaN(b)) {
      return Double.NaN;
    }

    double b2 = b*b;
    double L = Math.sqrt((1+a)*(1+a) + b2);
    double R = Math.sqrt((1-a)*(1-a) + b2);
    double A = (L - R)/2;
    double B = (L + R)/2;

    double re = Math.acos(A);
    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }

    double im = Math.log(B + Math.sqrt(B*B - 1));
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new BigComplex(re, signum*im);
  }
}
