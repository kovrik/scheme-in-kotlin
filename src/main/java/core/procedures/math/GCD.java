package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

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
  public Object apply(Object... args) {
    if (args != null) {
      if (args.length == 0) {
        return 0L;
      }
      Object result = args[0];
      if (!(NumberUtils.isRational(result))) {
        throw new WrongTypeException("Rational", result);
      }
      if (args.length == 1) {
        return ABS.apply1(args[0]);
      }
      for (int i = 1; i < args.length; i++) {
        Number first = (Number)result;
        if (!(NumberUtils.isRational(args[i]))) {
          throw new WrongTypeException("Rational", args[i]);
        }
        result = apply(first, (Number)args[i]);
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

  public static Number gcd(Double a, Double b) {
    if (a.isInfinite() || a.isNaN()) {
      throw new WrongTypeException("Integer", a);
    }
    if (b.isInfinite() || b.isNaN()) {
      throw new WrongTypeException("Integer", b);
    }
    if (a.longValue() != a || b.longValue() != b) {
      return ToInexact.toInexact(gcd((SCMBigRational)ToExact.toExact(a), (SCMBigRational)ToExact.toExact(b)));
    }
    return (double)gcd(a.longValue(), b.longValue());
  }

  public static Number gcd(BigDecimal a, BigDecimal b) {
    int scale = Math.max(a.scale(), b.scale());
    if (scale == 0) {
      return new BigDecimal(a.toBigInteger().gcd(b.toBigInteger()));
    } else {
      // TODO Check correctness
      return ToInexact.toInexact(gcd((BigDecimal)ToExact.toExact(a), (BigDecimal)ToExact.toExact(b)));
    }
  }

  public static BigInteger gcd(BigInteger a, BigInteger b) {
    return a.gcd(b);
  }

  public static SCMBigRational gcd(SCMBigRational first, SCMBigRational second) {
    return new SCMBigRational(first.getNumerator().gcd(second.getNumerator()),
                              LCM.lcm(first.getDenominator(), second.getDenominator()));
  }

  public Number apply(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return gcd((Long)first, (Long)second);
    }
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return gcd((SCMBigRational) first, (SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      return gcd(((SCMBigRational) first).toBigDecimal(), new BigDecimal(second.toString()));
    }
    if (second instanceof SCMBigRational) {
      return gcd(new BigDecimal(first.toString()), ((SCMBigRational) second).toBigDecimal());
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
