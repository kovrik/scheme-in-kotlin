package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public final class ToExact extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "inexact->exact";
  }

  @Override
  public Number apply1(Object arg) {
    return toExact(arg);
  }

  public static Number toExact(Object o) {
    /* Special cases */
    if (NumberUtils.isZero(o)) {
      if (o instanceof Double) {
        return 0d;
      } else if (o instanceof Float) {
        return 0f;
      } else if (o instanceof BigDecimal) {
        return BigDecimal.ZERO;
      }
    }
    if (o instanceof SCMBigComplex) {
      SCMBigComplex c = ((SCMBigComplex)o);
      return new SCMBigComplex(toExact(c.getRe()), toExact(c.getIm()));
    }
    if (o instanceof Float && (Float.isInfinite((Float) o) || Float.isNaN((Float) o))) {
      throw new ArithmeticException("No exact representation");
    }
    if (o instanceof Double) {
      if ((Double.isInfinite((Double) o) || Double.isNaN((Double) o))) {
        throw new ArithmeticException("No exact representation");
      }
      // FIXME There is no need to always call this method?
      return doubleToExact((Double)o);
    }
    if (o instanceof BigDecimal) {
      return bigDecimalToExact((BigDecimal) o);
    }
    return (Number) o;
  }

  private static Number doubleToExact(Double number) {
    long bits = Double.doubleToLongBits(number);
    long sign = bits >>> 63;
    long exponent = ((bits >>> 52) ^ (sign << 11)) - 1023;
    long fraction = bits << 12;
    long a = 1L;
    long b = 1L;
    for (int i = 63; i >= 12; i--) {
      a = a * 2 + ((fraction >>> i) & 1);
      b *= 2;
    }
    if (exponent > 0) {
      a *= 1 << exponent;
    } else {
      b *= 1 << -exponent;
    }
    if (sign == 1) {
      a *= -1;
    }
    return new SCMBigRational(BigInteger.valueOf(a), BigInteger.valueOf(b));
  }

  // FIXME Use the same approach as for Double?
  private static SCMBigRational bigDecimalToExact(BigDecimal number) {
    int scale = number.scale();
    if (scale > 0) {
      return new SCMBigRational(number.unscaledValue(), BigInteger.TEN.pow(scale));
    } else {
      return new SCMBigRational(number.unscaledValue().multiply(BigInteger.TEN.pow(-scale)), BigInteger.ONE);
    }
  }

}
