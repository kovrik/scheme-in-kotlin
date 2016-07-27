package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;
import java.math.BigInteger;

public class LCM extends AFn {

  @Override
  public String getName() {
    return "lcm";
  }

  private static long lcm(Long a, Long b) {
    if ((a.intValue() == 0) && (b.intValue() == 0)) {
      return 0L;
    }
    return (a / GCD.gcd(a, b)) * b;
  }

  private static Double lcm(Double a, Double b) {
    if ((a.intValue() == 0) && (b.intValue() == 0)) {
      return 0d;
    }
    return (a / GCD.gcd(a, b)) * b;
  }

  // FIXME Optmize
  private static BigDecimal lcm(BigDecimal a, BigDecimal b) {
    if ((BigDecimal.ZERO.compareTo(a) == 0) && (BigDecimal.ZERO.compareTo(b) == 0)) {
      return BigDecimal.ZERO;
    }
    // FIXME Check if numbers are integral!
    BigInteger gcd = a.toBigInteger().gcd(b.toBigInteger());
    return new BigDecimal((a.toBigIntegerExact().divide(gcd)).multiply(b.toBigIntegerExact()));
  }

  public Number invoke(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return lcm((Long)first, (Long)second);
    }

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return lcm((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return lcm((BigDecimal)first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return lcm(new BigDecimal(first.toString()), (BigDecimal)second);
    }

    return lcm(first.doubleValue(), second.doubleValue());
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
        if (args[0] instanceof Long) {
          return Math.abs((Long)args[0]);
        } else if (args[0] instanceof Double) {
          return Math.abs((Double) args[0]);
        } else if (args[0] instanceof BigDecimal) {
          return ((BigDecimal)args[0]).abs();
        }
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
}
