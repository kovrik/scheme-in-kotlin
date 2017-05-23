package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.math.Multiplication;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Cos extends AFn {

  public Cos() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
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
    if (Utils.INSTANCE.isZero(number)) {
      return 1L;
    }
    if (number instanceof BigDecimal) {
      return cos((BigDecimal)number);
    } else if (number instanceof BigInteger) {
      return cos((BigInteger)number);
    } else if (number instanceof BigComplex) {
      return Cos.cos((BigComplex)number);
    } else if (number instanceof BigRatio) {
      return cos(((BigRatio)number).toBigDecimal());
    }
    return Math.cos(number.doubleValue());
  }

  public static double cos(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cos(v);
    }
  }

  public static double cos(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cos(v);
    }
  }

  public static BigComplex cos(BigComplex c) {
    BigDecimal re = c.getRe();
    BigDecimal im = c.getIm();
    return new BigComplex(Multiplication.Companion.apply(Cos.cos(re), Cosh.cosh(im)),
                          Multiplication.Companion.apply(-1d, Multiplication.Companion.apply(Sin.sin(re), Sinh.sinh(im))));
  }
}
