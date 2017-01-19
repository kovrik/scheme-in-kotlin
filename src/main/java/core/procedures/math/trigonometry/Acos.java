package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public final class Acos extends AFn {

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
    if (arg instanceof Long) {
      double acos = Math.acos((Long) arg);
      if (Double.isNaN(acos)) {
        return acos(new SCMBigComplex((Number)arg));
      }
      return acos;
    } else if (arg instanceof Double) {
      double acos = Math.acos((Double) arg);
      if (Double.isNaN(acos)) {
        return acos(new SCMBigComplex((Number)arg));
      }
      return acos;
    } else if (arg instanceof BigDecimal) {
      return acos((BigDecimal)arg);
    } else if (arg instanceof SCMBigComplex) {
      return acos((SCMBigComplex)arg);
    } else {
      return acos(((SCMBigRational)arg).toBigDecimal());
    }
  }

  private static Number acos(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      double acos = Math.acos(v);
      if (Double.isNaN(acos)) {
        return acos(new SCMBigComplex(bd));
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
  private static Number acos(SCMBigComplex c) {
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
    return new SCMBigComplex(re, signum*im);
  }
}
