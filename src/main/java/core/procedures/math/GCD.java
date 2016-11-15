package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class GCD extends AFn {

  private static final Abs ABS = new Abs();

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "gcd";
  }

  @Override
  public Object invoke(Object... args) {
    if (args != null) {
      if (args.length == 0) {
        return 0L;
      }
      Object result = args[0];
      if (!(result instanceof Number)) {
        throw new WrongTypeException("Integer", result);
      }
      if (args.length == 1) {
        return ABS.invoke(args[0]);
      }
      for (int i = 1; i < args.length; i++) {
        Number first = (Number)result;
        if (!(args[i] instanceof Number)) {
          throw new WrongTypeException("Integer", args[i]);
        }
        result = invoke((Number)first, (Number)args[i]);
      }
      return result;
    }
    return 0L;
  }

  public static long gcd(Long a, Long b) {
    while (b > 0) {
      long temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  }

  public static Double gcd(Double a, Double b) {
    if (a.isInfinite() || a.isNaN() || a.longValue() != a) {
      throw new WrongTypeException("Integer", a);
    }
    if (b.isInfinite() || b.isNaN() || b.longValue() != b) {
      throw new WrongTypeException("Integer", b);
    }
    return (double)gcd(a.longValue(), b.longValue());
  }

  public static BigDecimal gcd(BigDecimal a, BigDecimal b) {
    // FIXME Check if numbers are integral?
    return new BigDecimal((a).toBigIntegerExact().gcd((b).toBigIntegerExact()));
  }

  public Number invoke(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return gcd((Long)first, (Long)second);
    }

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return gcd((BigDecimal) first, (BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return gcd((BigDecimal) first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return gcd(new BigDecimal(first.toString()), (BigDecimal) second);
    }

    return gcd(first.doubleValue(), second.doubleValue());
  }
}
