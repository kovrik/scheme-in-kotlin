package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;
import core.writer.Writer;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class ToExact extends AFn {

  private static final String NAME = "inexact->exact";

  public ToExact() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public Number apply1(Object arg) {
    return toExact(arg);
  }

  public static Number toExact(Object o) {
    /* Special cases */
    if (Utils.isZero(o)) {
      if (o instanceof Double) {
        return 0d;
      } else if (o instanceof Float) {
        return 0f;
      } else if (o instanceof BigDecimal) {
        return BigDecimal.ZERO;
      }
    }
    if (o instanceof BigComplex) {
      BigComplex c = ((BigComplex)o);
      return new BigComplex(toExact(c.getRe()), toExact(c.getIm()));
    }
    if (o instanceof Float) {
      Float f = (Float)o;
      if ((Float.isInfinite(f) || Float.isNaN(f))) {
        throw new ArithmeticException(NAME + ": no exact representation of: " + Writer.write(f));
      }
      /* Check if Double is integral */
      if (f == Math.floor(f)) {
        return f;
      }
      return doubleToExact(f.doubleValue());
    }
    if (o instanceof Double) {
      Double d = (Double)o;
      if ((Double.isInfinite(d) || Double.isNaN(d))) {
        throw new ArithmeticException(NAME + ": no exact representation of: " + Writer.write(d));
      }
      /* Check if Double is integral */
      if (d == Math.floor(d)) {
        return d;
      }
      return doubleToExact(d);
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
    return new BigRatio(BigInteger.valueOf(a), BigInteger.valueOf(b));
  }

  private static BigRatio bigDecimalToExact(BigDecimal number) {
    int scale = number.scale();
    if (scale > 0) {
      return new BigRatio(number.unscaledValue(), BigInteger.TEN.pow(scale));
    } else {
      return new BigRatio(number.unscaledValue().multiply(BigInteger.TEN.pow(-scale)), BigInteger.ONE);
    }
  }
}
