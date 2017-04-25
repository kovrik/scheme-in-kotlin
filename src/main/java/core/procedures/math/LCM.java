package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

import static core.procedures.math.GCD.gcd;

public final class LCM extends AFn {

  private static final Abs ABS = new Abs();

  public LCM() {
    super(new FnArgsBuilder().restArgsType(SCMBigRational.class));
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

  private SCMBigRational lcm(SCMBigRational first, SCMBigRational second) {
    return new SCMBigRational(lcm(first.getNumerator(), second.getNumerator()),
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
    if ((first instanceof Long) && (second instanceof Long)) {
      return lcm((Long)first, (Long)second);
    }
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return lcm((SCMBigRational) first, (SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      return lcm(((SCMBigRational) first).toBigDecimal(), NumberUtils.toBigDecimal(second));
    }
    if (second instanceof SCMBigRational) {
      return lcm(NumberUtils.toBigDecimal(first), ((SCMBigRational) second).toBigDecimal());
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return lcm((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return lcm((BigDecimal)first, NumberUtils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return lcm(NumberUtils.toBigDecimal(first), (BigDecimal)second);
    }
    return lcm(first.doubleValue(), second.doubleValue());
  }
}
