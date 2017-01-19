package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public final class Atan extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "atan";
  }

  @Override
  public Number apply1(Object arg) {
    /* Special cases */
    if (NumberUtils.isZero(arg)) {
      return 0L;
    }
    if (arg instanceof Long) {
      return Math.atan((Long) arg);
    } else if (arg instanceof Double) {
      return Math.atan((Double) arg);
    } else if (arg instanceof BigDecimal) {
      return atan((BigDecimal)arg);
    } else if (arg instanceof SCMBigComplex) {
      return atan((SCMBigComplex)arg);
    } else {
      return atan(((SCMBigRational)arg).toBigDecimal());
    }
  }

  public static double atan(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.atan(v);
    }
  }

  public static Number atan(SCMBigComplex c) {
    BigDecimal r = c.getRe();
    BigDecimal i = c.getIm();
    double a = r.doubleValue();
    if (Double.isInfinite(a) || Double.isNaN(a)) {
      return Double.NaN;
    }
    double b = i.doubleValue();
    if (Double.isInfinite(b) || Double.isNaN(b)) {
      return Double.NaN;
    }

    double a2 = a*a;
    double b2 = b*b;
    double re;
    if (r.compareTo(BigDecimal.ZERO) == 0 && (i.compareTo(BigDecimal.ONE.negate()) > 0) && (i.compareTo(BigDecimal.ONE) < 0)) {
      /* when x = 0 and -1 < y < 1 */
      re = 0;
    } else if (r.compareTo(BigDecimal.ZERO) == 0 && i.multiply(i).compareTo(BigDecimal.ONE) > 0) {
      /* when x = 0 and 1 < y^2
       * re(arctan(x + iy)) = pi/2 */
      re = Math.PI / 2;
    } else if (r.compareTo(BigDecimal.ZERO) > 0) {
      /* when x > 0
       * re(arctan(x + iy)) = pi/4 - (1/2) arctan ( (1 - x^2 - y^2)/(2x) ) */
      re = Math.PI/4 - 0.5*Math.atan((1 - a2 - b2) / (2*a));
    } else {
      /* when x < 0
       * re(arctan(x + iy)) = -pi/4 - (1/2) arctan ( (1 - x^2 - y^2)/(2x) ) */
      re = -Math.PI/4 - 0.5*Math.atan((1 - a2 - b2) / (2*a));
    }

    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }

    /* im(arctan(x + iy)) = -(1/4) ln ((1 - x^2 - y^2)^2 + (2x)^2) + (1/2) ln ((1 + y)^2 + x^2) */
    double im = -0.25*Math.log((1 - a2 - b2)*(1 - a2 - b2) + (4*a2)) + 0.5*Math.log((1 + b)*(1 + b) + a2);
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new SCMBigComplex(re, im);
  }
}
