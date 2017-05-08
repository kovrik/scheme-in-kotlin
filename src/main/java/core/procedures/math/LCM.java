package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

import static core.procedures.math.GCD.gcd;

public final class LCM extends AFn {

  private static final Abs ABS = new Abs();

  public LCM() {
    super(new FnArgsBuilder().rest(BigRatio.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "lcm";
  }

  @Override
  public Number apply(Object... args) {
    if (args.length == 0) {
      return 1L;
    }
    if (args.length == 1) {
      return ABS.apply1(args[0]);
    }
    Number result = (Number) args[0];
    for (int i = 1; i < args.length; i++) {
      result = lcm(result, (Number) args[i]);
    }
    return result;
  }

  private static long lcm(Long a, Long b) {
    if ((a.intValue() == 0) && (b.intValue() == 0)) {
      return 0L;
    }
    return (a / gcd(a, b)) * b;
  }

  private static Double lcm(Double a, Double b) {
    if ((a.intValue() == 0) && (b.intValue() == 0)) {
      return 0d;
    }
    return (a / gcd(a, b).doubleValue()) * b;
  }

  static BigInteger lcm(BigInteger first, BigInteger second) {
    return first.multiply(second.divide(gcd(first, second)));
  }

  private BigRatio lcm(BigRatio first, BigRatio second) {
    return BigRatio.valueOf(lcm(first.getNumerator(), second.getNumerator()),
                           gcd(first.getDenominator(), second.getDenominator()));
  }

  private Number lcm(BigDecimal a, BigDecimal b) {
    if ((BigDecimal.ZERO.compareTo(a) == 0) && (BigDecimal.ZERO.compareTo(b) == 0)) {
      return BigDecimal.ZERO;
    }
    int scale = Math.max(a.scale(), b.scale());
    if (scale == 0) {
      return new BigDecimal(lcm(a.toBigInteger(), b.toBigInteger()));
    } else {
      return ToInexact.toInexact(lcm((BigDecimal)ToExact.toExact(a), (BigDecimal) ToExact.toExact(b)));
    }
  }

  private Number lcm(Number first, Number second) {
    Number f = Utils.upcast(first);
    Number s = Utils.upcast(second);
    if ((f instanceof Long) && (s instanceof Long)) {
      return lcm((Long)first, (Long)second);
    }
    if ((first instanceof BigRatio) && (second instanceof BigRatio)) {
      return lcm((BigRatio) first, (BigRatio)second);
    }
    if (first instanceof BigRatio) {
      return lcm(((BigRatio) first).toBigDecimal(), Utils.toBigDecimal(second));
    }
    if (second instanceof BigRatio) {
      return lcm(Utils.toBigDecimal(first), ((BigRatio) second).toBigDecimal());
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return lcm((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return lcm((BigDecimal)first, Utils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return lcm(Utils.toBigDecimal(first), (BigDecimal)second);
    }
    if ((first instanceof BigInteger) && (second instanceof BigInteger)) {
      return lcm((BigInteger)first, (BigInteger)second);
    }
    if (first instanceof BigInteger) {
      return lcm((BigInteger)first, Utils.toBigInteger(second));
    }
    if (second instanceof BigInteger) {
      return lcm(Utils.toBigInteger(first), (BigInteger)second);
    }
    return lcm(first.doubleValue(), second.doubleValue());
  }
}
