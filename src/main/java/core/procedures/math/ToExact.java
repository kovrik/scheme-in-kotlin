package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.predicates.IsNumber;
import core.scm.SCMBigRational;

import java.math.BigDecimal;
import java.math.BigInteger;

public class ToExact extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "inexact->exact";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      return toExact(args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }

  public static Number toExact(Object o) {
    if (!IsNumber.isNumber(o)) {
      throw new WrongTypeException("Number", o);
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