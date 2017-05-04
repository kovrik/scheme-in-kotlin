package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.math.Multiplication;
import core.scm.BigComplex;
import core.scm.BigRational;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Sin extends AFn {

  public Sin() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sin";
  }

  @Override
  public Number apply1(Object arg) {
    return sin((Number)arg);
  }

  public static Number sin(Number number) {
    /* Special cases */
    if (Utils.isZero(number)) {
      return 0L;
    }
    if (number instanceof BigDecimal) {
      return sin((BigDecimal) number);
    } else if (number instanceof BigInteger) {
      return sin((BigInteger)number);
    } else if (number instanceof BigComplex) {
      return sin((BigComplex)number);
    } else if (number instanceof BigRational) {
      return sin(((BigRational)number).toBigDecimal());
    }
    return Math.sin(number.doubleValue());
  }

  public static double sin(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.sin(v);
    }
  }

  public static double sin(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.sin(v);
    }
  }

  public static BigComplex sin(BigComplex c) {
    BigDecimal re = c.getRe();
    BigDecimal im = c.getIm();
    return new BigComplex(Multiplication.apply(Sin.sin(re), Cosh.cosh(im)),
                          Multiplication.apply(Cos.cos(re), Sinh.sinh(im)));
  }
}
